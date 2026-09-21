#' Inspect a local CTTIR source suite without loading it
#'
#' Reads immediate child directories with DESCRIPTION files. Duplicate package
#' identities are retained and flagged: directory names are not package names.
#' Source presence and declared exports do not establish runtime qualification.
#' No source code, installation, services or network requests are executed.
#' @param root Existing suite directory.
#' @param check_installed Inspect installed package metadata as well (no loading).
#' @return Data frame of source paths, package names, versions, titles, declared
#'   literal exports, duplicate identity flags and optional installed versions.
#'   Malformed descriptions are retained with an error in `status`.
#' @export
#' @examples
#' bs_cttir_inventory(tempdir())
bs_cttir_inventory <- function(root, check_installed = FALSE) {
  if (!is.character(root) || length(root) != 1L || is.na(root) || !dir.exists(root))
    stop("root must be an existing directory", call. = FALSE)
  if (!is.logical(check_installed) || length(check_installed) != 1L || is.na(check_installed))
    stop("check_installed must be TRUE or FALSE", call. = FALSE)
  paths <- sort(list.dirs(root, recursive = FALSE, full.names = TRUE))
  paths <- paths[file.exists(file.path(paths, "DESCRIPTION"))]
  empty <- data.frame(directory = character(), path = character(), package = character(),
    version = character(), title = character(), exports = character(), status = character(),
    duplicate_package = logical(), installed_version = character())
  if (!length(paths)) return(empty)
  rows <- lapply(paths, function(p) {
    status <- "metadata_only"
    desc <- tryCatch(read.dcf(file.path(p, "DESCRIPTION")), error = function(e) NULL)
    field <- function(n) if (!is.null(desc) && nrow(desc) == 1L && n %in% colnames(desc)) desc[1L, n] else NA_character_
    pkg <- field("Package")
    if (is.na(pkg) || is.na(field("Version"))) status <- "invalid_description"
    ns <- file.path(p, "NAMESPACE")
    expr <- if (file.exists(ns)) tryCatch(parse(ns), error = function(e) NULL) else NULL
    ex <- unlist(lapply(expr, function(e) {
      if (is.call(e) && identical(e[[1L]], as.name("export"))) {
        args <- as.list(e)[-1L]
        vapply(args, function(a) if (is.symbol(a) || (is.character(a) && length(a) == 1L)) as.character(a) else "", character(1))
      } else character()
    }), use.names = FALSE)
    installed <- NA_character_
    if (check_installed && !is.na(pkg)) {
      location <- find.package(pkg, quiet = TRUE)
      if (length(location)) {
        ip <- tryCatch(read.dcf(file.path(location[1L], "DESCRIPTION")), error = function(e) NULL)
        if (!is.null(ip) && "Version" %in% colnames(ip)) installed <- ip[1L, "Version"]
      }
    }
    data.frame(directory = basename(p), path = normalizePath(p, winslash = "/"), package = pkg,
      version = field("Version"), title = field("Title"), exports = paste(sort(unique(ex[nzchar(ex)])), collapse = ";"),
      status = status, duplicate_package = FALSE, installed_version = installed)
  })
  out <- do.call(rbind, rows)
  out$duplicate_package <- !is.na(out$package) & (duplicated(out$package) | duplicated(out$package, fromLast = TRUE))
  rownames(out) <- NULL
  out
}
