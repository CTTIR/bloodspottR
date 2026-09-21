.bs_slides <- function(x) {
  if (inherits(x, "bs_result")) x$slides else if (is.data.frame(x)) x else
    .bs_abort("Expected bs_result or a measurement data frame", call. = FALSE)
}
.bs_metrics <- c("tissue_area_mm2", "red_area_mm2", "candidate_spots",
                 "unresolved_bulk_events", "operational_events")
.bs_parse_count_text <- function(x, name) {
  result <- rep(NA_real_, length(x))
  for (i in which(!is.na(x))) {
    raw <- trimws(x[i])
    if (!grepl("^[+]?[0-9]+([.][0-9]*)?([eE][+-]?[0-9]+)?$", raw))
      .bs_abort("Invalid numeric count: ", name, call. = FALSE)
    parts <- strsplit(sub("^[+]", "", raw), "[eE]")[[1L]]
    exponent <- if (length(parts) == 2L) suppressWarnings(as.numeric(parts[2L])) else 0
    fractional <- if (grepl(".", parts[1L], fixed = TRUE)) nchar(sub("^[^.]*[.]", "", parts[1L])) else 0
    digits <- sub("^0+", "", gsub(".", "", parts[1L], fixed = TRUE))
    if (!nzchar(digits)) { result[i] <- 0; next }
    shift <- exponent - fractional
    if (!is.finite(shift) || nchar(digits) + shift > 16 || nchar(digits) + shift < 1)
      .bs_abort("Count outside exact whole numeric range: ", name, call. = FALSE)
    if (shift < 0) {
      cut <- nchar(digits) + shift
      if (grepl("[1-9]", substring(digits, cut + 1)))
        .bs_abort("Count must be whole: ", name, call. = FALSE)
      digits <- substr(digits, 1, cut)
    } else if (shift > 0) digits <- paste0(digits, paste(rep("0", shift), collapse = ""))
    if (nchar(digits) == 16 && digits > "9007199254740992")
      .bs_abort("Count exceeds exact double precision: ", name, call. = FALSE)
    result[i] <- as.numeric(digits)
  }
  result
}
.bs_rates <- function(x) {
  den <- ifelse(!is.na(x$tissue_area_mm2) & x$tissue_area_mm2 > 0,
                x$tissue_area_mm2, NA_real_)
  x$red_percent <- 100 * (x$red_area_mm2 / den)
  x$spots_per_mm2 <- x$candidate_spots / den
  x$events_per_mm2 <- x$operational_events / den
  if (any(vapply(x[c("red_percent", "spots_per_mm2", "events_per_mm2")],
                 function(v) any(is.infinite(v) | is.nan(v)), logical(1))))
    .bs_abort("Derived rates exceed finite numeric range", call. = FALSE)
  x
}

#' Validate project or physical-slide measurement contracts
#'
#' Missing measurements remain missing. Numeric counts must be whole, nonnegative
#' doubles no larger than 2^53. Validation checks arithmetic and structure, not
#' biological accuracy or geometric independence of slides.
#' @param x A project, imported result, or canonical slide data frame.
#' @param level Currently `"structure"` only.
#' @return Invisibly `TRUE`, or an informative error.
#' @export
bs_validate <- function(x, level = "structure") {
  if (!identical(level, "structure")) .bs_abort("Only structure validation is supported", call. = FALSE)
  if (inherits(x, "bs_project")) {
    bs_project(x$path)
    return(invisible(TRUE))
  }
  d <- .bs_slides(x)
  if (anyDuplicated(names(d))) .bs_abort("Duplicate measurement column names", call. = FALSE)
  if (!nrow(d) || !all(c("physical_slide_id", .bs_metrics) %in% names(d)))
    .bs_abort("Required canonical measurement columns are missing or empty", call. = FALSE)
  if (!is.character(d$physical_slide_id) || anyNA(d$physical_slide_id) ||
      any(!nzchar(trimws(d$physical_slide_id))) || anyDuplicated(d$physical_slide_id))
    .bs_abort("physical_slide_id must contain unique nonempty strings", call. = FALSE)
  for (nm in .bs_metrics) {
    v <- d[[nm]]
    if (!is.numeric(v) || any(is.nan(v)) || any(!is.finite(v[!is.na(v)])) || any(v < 0, na.rm = TRUE))
      .bs_abort(nm, " must be nonnegative finite numbers or NA", call. = FALSE)
    if (nm %in% .bs_metrics[3:5] && any(v != floor(v) | v > 2^53, na.rm = TRUE))
      .bs_abort(nm, " must be exact whole counts no larger than 2^53", call. = FALSE)
  }
  if (any(d$red_area_mm2 > d$tissue_area_mm2, na.rm = TRUE))
    .bs_abort("Red area exceeds tissue area", call. = FALSE)
  if (any(d$candidate_spots > 2^53 - d$unresolved_bulk_events, na.rm = TRUE))
    .bs_abort("Combined event counts exceed exact double precision", call. = FALSE)
  if (any(d$operational_events != d$candidate_spots + d$unresolved_bulk_events, na.rm = TRUE))
    .bs_abort("Operational events must equal spots plus bulk events", call. = FALSE)
  if (any(d$tissue_area_mm2 == 0 & (d$candidate_spots > 0 | d$operational_events > 0), na.rm = TRUE))
    .bs_abort("Positive counts require positive tissue support", call. = FALSE)
  invisible(TRUE)
}

#' Import preserved result tables without changing source files
#'
#' Reads a canonical CSV, the legacy physical-slide `slides.csv`, or saved
#' `analysis.json` containing a `rows` table, or exported `results.json`.
#' A folder must contain exactly one
#' of these named files. The `p079` profile maps legacy area column names; it
#' does not impose study-specific groups or thresholds. Saved QC/group metadata
#' are preserved rather than inferred from signal. CSV identifiers are read as
#' strings, including numeric-looking identifiers with leading zeros.
#' @param path File or directory containing results.
#' @param profile Adapter profile, `"p079"` or `"canonical"`.
#' @return A `bs_result` object with canonical slide measurements, source SHA-256
#'   identity, and available saved metadata. SHA-256 is an integrity receipt, not an
#'   authenticity guarantee.
#' @export
bs_import_legacy <- function(path, profile = "p079") {
  .bs_scalar_string(path, "path")
  profile <- match.arg(profile, c("p079", "canonical"))
  if (dir.exists(path)) {
    candidates <- file.path(path, c("analysis.json", "results.json", "slides.csv", "results.csv"))
    candidates <- candidates[file.exists(candidates)]
    if (length(candidates) != 1L) .bs_abort("Folder must contain exactly one result source; supply a file", call. = FALSE)
    path <- candidates
  }
  if (!file.exists(path)) .bs_abort("Result source does not exist", call. = FALSE)
  metadata <- list()
  if (tolower(tools::file_ext(path)) == "json") {
    raw <- .bs_json(path)
    canonical <- !is.null(raw$slides)
    if (canonical && !identical(raw$schema_version, "1.0"))
      .bs_abort("Unsupported result schema", call. = FALSE)
    d <- if (canonical) raw$slides else raw$rows
    metadata <- raw[setdiff(names(raw), c("rows", "slides"))]
    if (!is.data.frame(d)) .bs_abort("JSON must contain a rows measurement table", call. = FALSE)
  } else if (tolower(tools::file_ext(path)) == "csv") {
    d <- utils::read.csv(path, colClasses = "character", check.names = FALSE,
                         na.strings = "")
  } else .bs_abort("Only CSV or JSON results are supported", call. = FALSE)
  if (anyDuplicated(names(d))) .bs_abort("Duplicate column names", call. = FALSE)
  if (profile == "p079") {
    for (pair in list(c("tissue_area_mm2", "tissue_mm2"), c("red_area_mm2", "red_mm2"))) {
      if (!pair[1] %in% names(d) && pair[2] %in% names(d)) d[[pair[1]]] <- d[[pair[2]]]
    }
  }
  for (nm in intersect(.bs_metrics, names(d))) {
    if (is.logical(d[[nm]]) && all(is.na(d[[nm]]))) d[[nm]] <- as.numeric(d[[nm]])
    if (is.character(d[[nm]])) {
      d[[nm]][!is.na(d[[nm]]) & d[[nm]] == "NA"] <- NA_character_
      val <- if (nm %in% .bs_metrics[3:5]) .bs_parse_count_text(d[[nm]], nm) else
        suppressWarnings(as.numeric(d[[nm]]))
      if (any(!is.na(d[[nm]]) & is.na(val))) .bs_abort("Invalid numeric measurement: ", nm, call. = FALSE)
      d[[nm]] <- val
    }
  }
  out <- structure(list(schema_version = "1.0", slides = d, profile = profile,
    source = list(path = normalizePath(path), sha256 = digest::digest(file = path, algo = "sha256")),
    metadata = metadata,
    provenance = if (is.list(metadata$provenance)) metadata$provenance else metadata,
    groups = if (is.data.frame(metadata$groups)) metadata$groups else data.frame(),
    comparison = if (is.data.frame(metadata$comparison)) metadata$comparison else NULL), class = "bs_result")
  bs_validate(out)
  out$slides <- .bs_rates(d)
  out
}

#' Pool physical-slide measurements by declared groups
#'
#' Rates divide summed numerators by summed tissue area, never average slide
#' rates. Missing inputs propagate. Unknown group labels form explicit NA groups.
#' Saved control labels are carried without interpreting biological identity.
#' @param x Imported results or canonical physical-slide measurements.
#' @param strata Character column names to group by. `NULL` gives one overall row.
#' @param qc_revision Optional data frame keyed uniquely by `physical_slide_id`.
#'   It must cover exactly the measured slides and may replace metadata, but
#'   cannot replace measurements. The source object is not modified.
#' @return Data frame with summed measurements, pooled rates, slide counts and
#'   a list column `slide_ids`. Only observed combinations are returned.
#' @export
bs_summarize <- function(x, strata = NULL, qc_revision = NULL) {
  bs_validate(x)
  d <- .bs_slides(x)
  if (!is.null(qc_revision)) {
    q <- qc_revision
    if (!is.data.frame(q) || !"physical_slide_id" %in% names(q) ||
        anyNA(q$physical_slide_id) || anyDuplicated(q$physical_slide_id) ||
        !setequal(q$physical_slide_id, d$physical_slide_id) ||
        anyDuplicated(names(q))) .bs_abort("QC revision must uniquely cover all measured slides", call. = FALSE)
    protected <- c(.bs_metrics, "tissue_mm2", "red_mm2", "red_percent", "spots_per_mm2", "events_per_mm2")
    cols <- setdiff(names(q), "physical_slide_id")
    if (any(cols %in% protected)) .bs_abort("QC revision cannot replace measurement columns", call. = FALSE)
    d[cols] <- q[match(d$physical_slide_id, q$physical_slide_id), cols, drop = FALSE]
  }
  if (!is.null(strata) && (!is.character(strata) || anyNA(strata) ||
      anyDuplicated(strata) || !all(strata %in% names(d)) || any(strata %in% c(.bs_metrics, "n_slides", "slide_ids", "red_percent", "spots_per_mm2", "events_per_mm2"))))
    .bs_abort("strata must name distinct metadata columns", call. = FALSE)
  if (length(strata) && any(vapply(d[strata], is.list, logical(1))))
    .bs_abort("Grouping columns must be atomic", call. = FALSE)
  keys <- if (length(strata)) unique(d[strata]) else data.frame(.overall = "all")
  ans <- lapply(seq_len(nrow(keys)), function(i) {
    selected <- rep(TRUE, nrow(d))
    for (nm in strata) {
      key <- keys[[nm]][i]
      selected <- selected & if (is.na(key)) is.na(d[[nm]]) else !is.na(d[[nm]]) & d[[nm]] == key
    }
    z <- d[selected, , drop = FALSE]
    row <- if (length(strata)) keys[i, , drop = FALSE] else data.frame(group = "all")
    row$n_slides <- nrow(z)
    for (nm in .bs_metrics) {
      values <- z[[nm]]
      if (nm %in% .bs_metrics[3:5] && !anyNA(values)) {
        remaining <- 2^53
        for (value in sort(values, decreasing = TRUE)) {
          if (value > remaining) .bs_abort("Pooled counts exceed exact double precision", call. = FALSE)
          remaining <- remaining - value
        }
      }
      row[[nm]] <- sum(values)
      if (is.infinite(row[[nm]])) .bs_abort("Pooled measurements exceed finite numeric range", call. = FALSE)
    }
    row$slide_ids <- I(list(z$physical_slide_id))
    .bs_rates(row)
  })
  result <- do.call(rbind, ans)
  rownames(result) <- NULL
  result
}

#' Compare two runs with explicit support checks
#'
#' `common` aligns shared physical slides and requires identical `support_id`,
#' or identical legacy `regions` and `acquired_pixels`. This verifies declared
#' support identity, not spatial registration. It never rescales to fabricate a
#' common support. `union` retains unmatched slides with missing values and
#' reports whether comparable support was documented.
#' @param runs Named list of exactly two imported results or canonical tables.
#' @param support `"common"` or `"union"`.
#' @return Data frame with baseline/run values, second-minus-first differences
#'   and `support_verified`. Run names and excluded IDs are attributes.
#' @export
bs_compare <- function(runs, support = "common") {
  support <- match.arg(support, c("common", "union"))
  if (!is.list(runs) || length(runs) != 2L || is.null(names(runs)) ||
      anyNA(names(runs)) || any(!nzchar(names(runs))) || anyDuplicated(names(runs)))
    .bs_abort("runs must be a named list of exactly two results", call. = FALSE)
  lapply(runs, bs_validate)
  a <- .bs_rates(.bs_slides(runs[[1]])); b <- .bs_rates(.bs_slides(runs[[2]]))
  ids <- if (support == "common") intersect(a$physical_slide_id, b$physical_slide_id) else
    union(a$physical_slide_id, b$physical_slide_id)
  if (!length(ids)) .bs_abort("No slides available on requested support", call. = FALSE)
  ia <- match(ids, a$physical_slide_id); ib <- match(ids, b$physical_slide_id)
  fields <- if (all(vapply(list(a, b), function(z) "support_id" %in% names(z), logical(1))))
    "support_id" else c("regions", "acquired_pixels")
  verified <- rep(FALSE, length(ids))
  if (all(fields %in% names(a)) && all(fields %in% names(b))) {
    verified <- !is.na(ia) & !is.na(ib)
    for (nm in fields) {
      va <- as.character(a[[nm]][ia]); vb <- as.character(b[[nm]][ib])
      verified <- verified & !is.na(va) & !is.na(vb) & nzchar(va) & nzchar(vb) & va == vb
    }
  }
  if (support == "common" && any(!verified))
    .bs_abort("Common support is unknown or differs; reconcile support or use union", call. = FALSE)
  out <- data.frame(physical_slide_id = ids, support_verified = verified)
  for (nm in c(.bs_metrics, "red_percent", "spots_per_mm2", "events_per_mm2")) {
    out[[paste0(nm, "_first")]] <- a[[nm]][ia]
    out[[paste0(nm, "_second")]] <- b[[nm]][ib]
    out[[paste0(nm, "_delta")]] <- ifelse(verified, b[[nm]][ib] - a[[nm]][ia], NA_real_)
  }
  attr(out, "runs") <- names(runs)
  attr(out, "excluded_ids") <- list(first = setdiff(a$physical_slide_id, ids), second = setdiff(b$physical_slide_id, ids))
  out
}
