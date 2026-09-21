.bs_scalar_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x))
    stop(name, " must be one nonempty string", call. = FALSE)
}
.bs_json <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("Install jsonlite to read or write JSON manifests", call. = FALSE)
  jsonlite::fromJSON(path, simplifyVector = TRUE, bigint_as_char = TRUE)
}

#' Create or open a portable analysis project
#'
#' Creation writes a versioned JSON manifest only to a new, empty directory.
#' Opening never mutates files and does not start analysis services.
#' @param path Project directory.
#' @param create Whether to create a new project.
#' @return A `bs_project` handle with its normalized root and manifest.
#' @export
bs_project <- function(path, create = FALSE) {
  .bs_scalar_string(path, "path")
  if (!is.logical(create) || length(create) != 1L || is.na(create))
    stop("create must be TRUE or FALSE", call. = FALSE)
  manifest_path <- file.path(path, "bloodspottr-project.json")
  if (create) {
    if (!requireNamespace("jsonlite", quietly = TRUE))
      stop("Install jsonlite to create a project", call. = FALSE)
    if (file.exists(path) && !dir.exists(path)) stop("path is not a directory", call. = FALSE)
    if (dir.exists(path) && length(list.files(path, all.files = TRUE, no.. = TRUE)))
      stop("Creation requires an empty directory; existing work is preserved", call. = FALSE)
    if (!dir.exists(path) && !dir.create(path, recursive = TRUE))
      stop("Could not create project directory", call. = FALSE)
    manifest <- list(schema_version = "1.0", project_id = basename(normalizePath(path)),
                     created_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                     results = list(), scientific_status = "exploratory")
    staging <- tempfile("manifest-", tmpdir = path)
    on.exit(unlink(staging), add = TRUE)
    jsonlite::write_json(manifest, staging, auto_unbox = TRUE, pretty = TRUE)
    if (!file.rename(staging, manifest_path)) stop("Manifest commit failed", call. = FALSE)
  }
  if (!file.exists(manifest_path)) stop("No bloodspottr-project.json found", call. = FALSE)
  manifest <- .bs_json(manifest_path)
  if (!identical(manifest$schema_version, "1.0") ||
      !is.character(manifest$project_id) || length(manifest$project_id) != 1L ||
      is.na(manifest$project_id) || !nzchar(manifest$project_id))
    stop("Invalid or unsupported project manifest", call. = FALSE)
  structure(list(path = normalizePath(path), manifest = manifest), class = "bs_project")
}

#' Inspect analysis state without launching workers
#' @param x A project or imported result object.
#' @return A list describing schema, object kind and available results.
#' @export
bs_status <- function(x) {
  bs_validate(x)
  if (inherits(x, "bs_project")) return(list(kind = "project", path = x$path,
    schema_version = x$manifest$schema_version,
    registered_results = length(x$manifest$results), running_jobs = NA_integer_,
    note = "Worker state is not inferred from saved manifests"))
  list(kind = "results", schema_version = x$schema_version, slides = nrow(.bs_slides(x)),
       scientific_status = "exploratory", source = if (is.list(x)) x$source else NULL)
}
