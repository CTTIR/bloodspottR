#' Export a frozen set of result tables
#'
#' Creates a new directory with CSV tables, optional Excel output and a SHA-256
#' manifest. Existing destinations are never overwritten. CSV character values
#' beginning with spreadsheet formula characters are escaped for safe viewing;
#' canonical JSON preserves the original strings.
#' @param results A validated bloodspottR result object.
#' @param out A new output directory.
#' @param xlsx Include an Excel workbook (requires openxlsx2).
#' @return The normalized delivery directory, invisibly.
#' @export
bs_export_results <- function(results, out, xlsx = FALSE) {
  .bs_export_check(results, out)
  if (!is.logical(xlsx) || length(xlsx) != 1L || is.na(xlsx))
    stop("xlsx must be TRUE or FALSE", call. = FALSE)
  if (xlsx && !requireNamespace("openxlsx2", quietly = TRUE))
    stop("Excel export requires openxlsx2", call. = FALSE)
  tables <- list(Slides = results$slides)
  if (is.data.frame(results$groups)) tables$Groups <- results$groups
  tables <- lapply(tables, .bs_flat_table)
  tables$Methods <- data.frame(item = c("Status", "Units", "Scope"),
    value = c("Exploratory image-derived candidates", "Areas: mm2; densities: per mm2",
              "Slide summaries; raw images and individual annotations are separate artifacts"))
  parent <- dirname(out)
  if (!dir.exists(parent)) stop("Parent directory does not exist", call. = FALSE)
  stage <- tempfile(".bloodspottr-export-", tmpdir = parent)
  if (!dir.create(stage)) stop("Could not create export staging directory", call. = FALSE)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  for (name in names(tables)) {
    safe <- tables[[name]]
    for (column in names(safe)) {
      if (is.character(safe[[column]])) {
        bad <- !is.na(safe[[column]]) & grepl("^[[:space:]]*[=+@-]", safe[[column]])
        safe[[column]][bad] <- paste0("'", safe[[column]][bad])
      }
    }
    utils::write.csv(safe, file.path(stage, paste0(name, ".csv")), row.names = FALSE, na = "")
  }
  jsonlite::write_json(unclass(results), file.path(stage, "results.json"), auto_unbox = TRUE,
                       pretty = TRUE, dataframe = "rows", digits = NA, null = "null", na = "null")
  if (xlsx) {
    workbook <- openxlsx2::wb_workbook()
    for (name in names(tables)) {
      workbook$add_worksheet(name)
      if (ncol(tables[[name]])) workbook$add_data(name, tables[[name]])
    }
    workbook$save(file.path(stage, "results.xlsx"))
  }
  .bs_manifest(stage)
  if (file.exists(out) || !file.rename(stage, out)) stop("Could not commit export directory", call. = FALSE)
  invisible(normalizePath(out, winslash = "/", mustWork = TRUE))
}

.bs_export_check <- function(results, out) {
  if (!inherits(results, "bs_result") || !is.data.frame(results$slides))
    stop("results must be a bs_result", call. = FALSE)
  bs_validate(results)
  if (!is.character(out) || length(out) != 1L || is.na(out) || !nzchar(out))
    stop("out must be one nonempty path", call. = FALSE)
  if (file.exists(out) || dir.exists(out)) stop("Output already exists", call. = FALSE)
}

.bs_manifest <- function(path) {
  files <- list.files(path, recursive = TRUE, full.names = TRUE)
  files <- files[!file.info(files)$isdir]
  hashes <- vapply(files, digest::digest, "", file = TRUE, algo = "sha256")
  relative <- substring(files, nchar(path) + 2L)
  writeLines(paste(hashes, relative, sep = "  "), file.path(path, "SHA256SUMS"))
}

.bs_html <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

.bs_html_table <- function(x) {
  if (!nrow(x)) return("<p>No rows in this table.</p>")
  x <- .bs_flat_table(x)
  x[] <- lapply(x, function(v) { v <- as.character(v); v[is.na(v)] <- "Not available"; v })
  rows <- apply(x, 1L, function(r) paste0("<tr>", paste0("<td>", .bs_html(r), "</td>", collapse = ""), "</tr>"))
  paste0("<div class='table-scroll'><table><thead><tr>",
    paste0("<th scope='col'>", .bs_html(names(x)), "</th>", collapse = ""),
    "</tr></thead><tbody>", paste(rows, collapse = "\n"), "</tbody></table></div>")
}

#' Render a portable HTML results report
#'
#' Writes a self-contained report and machine-readable tables to a new directory.
#' This initial renderer reports supplied summaries and provenance; it does not
#' invent training methods or embed unavailable original images. Print the HTML
#' from a browser when a portable PDF is needed. The study-specific full-slide
#' TeX renderer remains a separately qualified backend.
#' @param results A bloodspottR result object.
#' @param out A new delivery directory.
#' @param title Report title.
#' @param author Report author, explicitly supplied by the caller.
#' @param background Curated background text, treated as plain text.
#' @param xlsx Include the workbook.
#' @return The report path, invisibly.
#' @export
bs_report <- function(results, out, title = "Histology burden report",
                       author = "", background = "", xlsx = FALSE) {
  for (x in list(title, author, background))
    if (!is.character(x) || length(x) != 1L || is.na(x))
      stop("Report text must be scalar character values", call. = FALSE)
  .bs_export_check(results, out)
  parent <- dirname(out)
  if (!dir.exists(parent)) stop("Parent directory does not exist", call. = FALSE)
  # The final destination appears only after every report artifact succeeds.
  stage <- tempfile(".bloodspottr-report-", tmpdir = parent)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  bs_export_results(results, stage, xlsx = xlsx)
  provenance <- jsonlite::toJSON(results$provenance, auto_unbox = TRUE, pretty = TRUE,
                               null = "null", na = "null")
  groups <- if (is.data.frame(results$groups)) results$groups else data.frame()
  html <- paste0("<!doctype html><html lang='en'><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width,initial-scale=1'>",
    "<title>", .bs_html(title), "</title><style>",
    "body{font:16px/1.6 system-ui,sans-serif;color:#173245;background:#f4f7fa;margin:0}",
    "main{max-width:1200px;margin:2rem auto;padding:2rem;background:white}",
    "h1{font-size:2.3rem}h2{margin-top:2.4rem}.tag{color:#72510a;background:#fff2cc;padding:1rem}",
    ".table-scroll{overflow:auto}table{border-collapse:collapse;width:100%;font-size:.85rem}",
    "th,td{padding:.6rem;border-bottom:1px solid #dce3ea;text-align:left}th{background:#eaf1f6}",
    "pre{white-space:pre-wrap;overflow-wrap:anywhere}a{color:#075b89}",
    "@media print{body{background:white}main{margin:0;padding:0}.table-scroll{overflow:visible}}",
    "</style></head><body><main><header><h1>", .bs_html(title), "</h1><p>",
    .bs_html(author), "</p></header><p class='tag'>Exploratory image-derived candidates; software checks do not establish biological accuracy.</p>",
    "<section><h2>Background</h2><p>", .bs_html(background), "</p></section>",
    "<section><h2>Grouped results</h2>", .bs_html_table(groups), "</section>",
    "<section><h2>Slide results</h2>", .bs_html_table(results$slides), "</section>",
    "<section><h2>Methods and provenance</h2><pre>", .bs_html(provenance), "</pre></section>",
    "<section><h2>Data</h2><p><a href='results.json'>Canonical results</a> \u00b7 ",
    "<a href='Slides.csv'>Slide CSV</a> \u00b7 <a href='SHA256SUMS'>Checksums</a></p>",
    "<p>These are summary tables. Images, pixel masks, individual events and training weights are not embedded.</p></section></main></body></html>")
  writeLines(html, file.path(stage, "report.html"), useBytes = TRUE)
  unlink(file.path(stage, "SHA256SUMS"))
  .bs_manifest(stage)
  if (file.exists(out) || !file.rename(stage, out)) stop("Could not commit report directory", call. = FALSE)
  invisible(normalizePath(file.path(out, "report.html"), winslash = "/", mustWork = TRUE))
}

# Preserve nested metadata as JSON cells in rectangular presentation formats.
.bs_flat_table <- function(x) {
  for (nm in names(x)) {
    v <- x[[nm]]
    if (is.data.frame(v)) {
      x[[nm]] <- vapply(seq_len(nrow(v)), function(i)
        as.character(jsonlite::toJSON(v[i, , drop = FALSE], dataframe = "rows",
          auto_unbox = TRUE, null = "null", na = "null", digits = NA)), "")
    } else if (is.list(v)) {
      x[[nm]] <- vapply(v, function(value)
        as.character(jsonlite::toJSON(value, auto_unbox = TRUE,
          null = "null", na = "null", digits = NA)), "")
    } else if (is.factor(v)) x[[nm]] <- as.character(v)
  }
  x
}
