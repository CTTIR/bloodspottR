#' Configure an external analysis worker
#'
#' The explicitly trusted executable receives two final arguments: a JSON request
#' path and an output staging directory. It must write result.json with
#' schema_version 1, status 'complete', and a nonempty artifacts array of relative
#' file paths. A subprocess is isolated and cleaned up after completion or timeout.
#' No worker is installed or started by this constructor.
#' @param command Path or PATH name of a trusted executable.
#' @param args Character arguments preceding the request/output paths.
#' @param version Explicit worker version or source commit identifier.
#' @param operations Supported operations, such as train or analyze.
#' @return A bs_backend specification.
#' @export
bs_backend <- function(command, args = character(), version,
                       operations = c("train", "analyze")) {
  if (!is.character(command) || length(command) != 1L || is.na(command) || !nzchar(command))
    stop("command must identify one executable", call. = FALSE)
  executable <- if (file.exists(command)) normalizePath(command, winslash = "/") else Sys.which(command)
  if (!nzchar(executable) || dir.exists(executable) || file.access(executable, 1L) != 0L) stop("Executable not found", call. = FALSE)
  if (!is.character(args) || anyNA(args)) stop("args must be character", call. = FALSE)
  if (!is.character(version) || length(version) != 1L || is.na(version) || !nzchar(version))
    stop("An explicit backend version is required", call. = FALSE)
  if (!is.character(operations) || !length(operations) || anyNA(operations) ||
      any(!grepl("^[a-z][a-z0-9_]*$", operations)) || anyDuplicated(operations))
    stop("operations must be unique operation names", call. = FALSE)
  structure(list(command = unname(executable), args = args, version = version,
                 operations = operations), class = "bs_backend")
}

#' Run a validated external worker transaction
#' @param backend A bs_backend object.
#' @param operation A supported operation name.
#' @param inputs Named character vector of existing input files.
#' @param parameters Named list of JSON-serializable worker settings.
#' @param out New output directory. It is committed only after validation.
#' @param timeout Maximum elapsed seconds for the worker.
#' @return A job receipt with status, backend, input hashes and output hashes.
#' @export
bs_run_backend <- function(backend, operation, inputs, parameters = list(), out,
                           timeout = 3600) {
  if (!inherits(backend, "bs_backend") || !is.list(backend)) stop("Expected a bs_backend", call. = FALSE)
  backend <- bs_backend(backend$command, backend$args, backend$version, backend$operations)
  if (!is.character(operation) || length(operation) != 1L || is.na(operation) ||
      !operation %in% backend$operations) stop("Unsupported operation", call. = FALSE)
  if (!is.character(inputs) || !length(inputs) || anyNA(inputs) ||
      is.null(names(inputs)) || anyNA(names(inputs)) || any(!nzchar(trimws(names(inputs)))) || anyDuplicated(names(inputs)) ||
      any(!file.exists(inputs)) || any(dir.exists(inputs)))
    stop("inputs must be named existing files", call. = FALSE)
  if (!is.list(parameters) || (length(parameters) &&
      (is.null(names(parameters)) || anyNA(names(parameters)) || any(!nzchar(trimws(names(parameters)))) || anyDuplicated(names(parameters)))))
    stop("parameters must be a named list", call. = FALSE)
  valid_parameter <- function(x) {
    if (is.null(x)) return(TRUE)
    if (is.list(x)) {
      if (!is.null(names(x)) && (anyNA(names(x)) || anyDuplicated(names(x)) || any(!nzchar(names(x))))) return(FALSE)
      return(all(vapply(x, valid_parameter, logical(1))))
    }
    if (is.object(x)) return(FALSE)
    if (is.numeric(x) && !is.complex(x)) return(all(is.finite(x)))
    (is.character(x) || is.logical(x)) && !anyNA(x)
  }
  if (!valid_parameter(parameters))
    stop("parameters must contain finite JSON-compatible values; use NULL for explicit null", call. = FALSE)
  if (!is.numeric(timeout) || length(timeout) != 1L || !is.finite(timeout) || timeout <= 0)
    stop("timeout must be finite and positive", call. = FALSE)
  if (!is.character(out) || length(out) != 1L || is.na(out) || !nzchar(out) ||
      file.exists(out) || !dir.exists(dirname(out))) stop("out must be a new path in an existing directory", call. = FALSE)
  if (!requireNamespace("processx", quietly = TRUE)) stop("Worker execution requires processx", call. = FALSE)
  stage <- tempfile(".bloodspottr-worker-", tmpdir = dirname(out))
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  inputs <- vapply(inputs, normalizePath, "", winslash = "/", mustWork = TRUE)
  hashes <- vapply(inputs, digest::digest, "", file = TRUE, algo = "sha256")
  request <- list(schema_version = 1L, operation = operation, backend_version = backend$version,
                  inputs = as.list(inputs), input_sha256 = as.list(hashes), parameters = parameters)
  request_file <- tempfile("bloodspottr-request-", fileext = ".json")
  on.exit(unlink(request_file), add = TRUE)
  jsonlite::write_json(request, request_file, auto_unbox = TRUE, pretty = TRUE, digits = NA)
  request_hash <- digest::digest(file = request_file, algo = "sha256")
  process <- processx::run(backend$command, c(backend$args, request_file, normalizePath(stage)),
                           timeout = timeout, error_on_status = FALSE, cleanup_tree = TRUE,
                           stdout = "|", stderr = "|")
  if (isTRUE(process$timeout)) stop("Worker timed out; owned process tree cleaned up", call. = FALSE)
  if (process$status != 0L) stop(paste("Worker failed:", process$stderr), call. = FALSE)
  manifest <- file.path(stage, "result.json")
  if (!file.exists(manifest) || dir.exists(manifest)) stop("Worker did not produce result.json", call. = FALSE)
  staged_files <- list.files(stage, recursive = TRUE, full.names = TRUE, all.files = TRUE,
                            include.dirs = TRUE, no.. = TRUE)
  prefix <- paste0(normalizePath(stage, winslash = "/"), "/")
  if (any(!startsWith(normalizePath(staged_files, winslash = "/", mustWork = TRUE), prefix)) ||
      any(nzchar(Sys.readlink(staged_files))))
    stop("Worker output contains links or paths escaping output directory", call. = FALSE)
  result <- jsonlite::read_json(manifest, simplifyVector = FALSE)
  if (!is.list(result) || !identical(result$status, "complete") ||
      !isTRUE(result$schema_version == 1L)) stop("Worker completion contract failed", call. = FALSE)
  raw_files <- result$artifacts
  if (!is.list(raw_files) || !is.null(names(raw_files)) || !length(raw_files) ||
      any(!vapply(raw_files, function(x) is.character(x) && length(x) == 1L && !is.na(x), logical(1))))
    stop("Worker artifacts must be a nonempty array of paths", call. = FALSE)
  files <- unlist(raw_files, use.names = FALSE)
  if (!is.character(files) || !length(files) || anyNA(files) || any(!nzchar(files)) || anyDuplicated(files) ||
      any(grepl("(^[/\\\\]|^[A-Za-z]:|(^|[/\\\\])\\.\\.([/\\\\]|$))", files)))
    stop("Worker artifact paths are invalid", call. = FALSE)
  full <- file.path(stage, files)
  if (any(!file.exists(full)) || any(dir.exists(full))) stop("Worker artifacts are missing", call. = FALSE)
  resolved <- normalizePath(full, winslash = "/", mustWork = TRUE)
  prefix <- paste0(normalizePath(stage, winslash = "/"), "/")
  if (anyDuplicated(resolved)) stop("Worker artifact paths alias the same file", call. = FALSE)
  if (any(basename(resolved) == "job-receipt.json")) stop("Worker artifact uses reserved receipt name", call. = FALSE)
  if (any(!startsWith(resolved, prefix))) stop("Worker artifacts escape output directory", call. = FALSE)
  if (!file.exists(request_file) || !identical(request_hash, digest::digest(file = request_file, algo = "sha256")))
    stop("Worker changed the request manifest", call. = FALSE)
  after_hash <- vapply(inputs, digest::digest, "", file = TRUE, algo = "sha256")
  if (!identical(hashes, after_hash)) stop("Worker changed an input file", call. = FALSE)
  receipt <- list(schema_version = 1L, status = "complete", operation = operation,
    backend = unclass(backend), request = request,
    request_sha256 = request_hash,
    input_sha256 = as.list(hashes),
    artifacts = stats::setNames(as.list(vapply(full, digest::digest, "", file = TRUE, algo = "sha256")), files),
    stdout = process$stdout, stderr = process$stderr,
    completed_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  jsonlite::write_json(receipt, file.path(stage, "job-receipt.json"), auto_unbox = TRUE, pretty = TRUE)
  if (file.exists(out) || !file.rename(stage, out)) stop("Could not commit worker output", call. = FALSE)
  receipt$output <- normalizePath(out, winslash = "/", mustWork = TRUE)
  structure(receipt, class = "bs_job")
}

#' Train using a configured worker
#' @inheritParams bs_run_backend
#' @return A validated bs_job receipt. Model accuracy is not established by this receipt.
#' @export
bs_train <- function(inputs, backend, out, parameters = list(), timeout = 3600) {
  bs_run_backend(backend, "train", inputs, parameters, out, timeout)
}

#' Analyze using a configured worker
#' @inheritParams bs_run_backend
#' @return A validated bs_job receipt. Whole-slide capability depends on the worker.
#' @export
bs_analyze <- function(inputs, backend, out, parameters = list(), timeout = 3600) {
  bs_run_backend(backend, "analyze", inputs, parameters, out, timeout)
}
