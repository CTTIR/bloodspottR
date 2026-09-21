check_nonnegative <- function(x, name, integer = FALSE) {
  if (!is.numeric(x) || anyNA(x) || any(!is.finite(x)) || any(x < 0))
    stop(name, " must be finite nonnegative numbers", call. = FALSE)
  if (integer && any(x != floor(x) | x > 2^53))
    stop(name, " must contain exact whole counts no larger than 2^53", call. = FALSE)
}

.bs_sum_exact_counts <- function(x) {
  if (anyNA(x)) return(NA_real_)
  remaining <- 2^53
  for (value in sort(x, decreasing = TRUE)) {
    if (value > remaining) stop("Pooled counts exceed exact double precision", call. = FALSE)
    remaining <- remaining - value
  }
  sum(x)
}

#' Calculate calibrated burden from classified pixels and objects
#'
#' Inputs must already refer to the same evaluable tissue mask. This function
#' checks units and arithmetic; it does not validate image segmentation.
#' Zero evaluable area produces undefined densities, represented by `NA`.
#' @param n_spots Nonnegative whole spot counts.
#' @param red_pixels Nonnegative whole positive-pixel counts within tissue.
#' @param tissue_pixels Nonnegative whole evaluable-tissue pixel counts.
#' @param pixel_width_um,pixel_height_um Pixel dimensions in micrometres; either
#'   one value or one per measurement.
#' @param n_profiles Optional nonnegative whole counts of resolved profiles.
#'   Omit when profile counts are unavailable or unsupported.
#' @return A data frame of areas in square millimetres, counts, fractions and
#'   densities. The `n_profiles` and `profiles_per_mm2` columns are `NA` when absent.
#' @export
#' @examples
#' measure_burden(10, 200, 10000, 0.25, 0.25)
measure_burden <- function(n_spots, red_pixels, tissue_pixels,
                           pixel_width_um, pixel_height_um, n_profiles = NULL) {
  n <- length(tissue_pixels)
  if (!n || length(n_spots) != n || length(red_pixels) != n)
    stop("Counts must have equal, nonzero lengths", call. = FALSE)
  check_nonnegative(n_spots, "n_spots", TRUE)
  check_nonnegative(red_pixels, "red_pixels", TRUE)
  check_nonnegative(tissue_pixels, "tissue_pixels", TRUE)
  expand_scale <- function(value, name) {
    check_nonnegative(value, name)
    if (!length(value) %in% c(1L, n) || any(value == 0))
      stop(name, " must be positive with length one or the measurement count", call. = FALSE)
    rep(value, length.out = n)
  }
  sx <- expand_scale(pixel_width_um, "pixel_width_um")
  sy <- expand_scale(pixel_height_um, "pixel_height_um")
  if (any(red_pixels > tissue_pixels))
    stop("Positive pixels exceed evaluable tissue", call. = FALSE)
  if (is.null(n_profiles)) {
    n_profiles <- rep(NA_real_, n)
  } else {
    if (length(n_profiles) != n) stop("n_profiles has incompatible length", call. = FALSE)
    check_nonnegative(n_profiles, "n_profiles", TRUE)
  }
  if (any(tissue_pixels == 0 & (n_spots > 0 | (!is.na(n_profiles) & n_profiles > 0))))
    stop("Object counts cannot be positive without evaluable tissue", call. = FALSE)
  if (any(n_spots > red_pixels) || any(red_pixels == 0 & !is.na(n_profiles) & n_profiles > 0))
    stop("Positive objects require supporting positive pixels", call. = FALSE)
  tissue_area_mm2 <- tissue_pixels * sx * sy / 1e6
  red_area_mm2 <- red_pixels * sx * sy / 1e6
  if (any(!is.finite(tissue_area_mm2) | !is.finite(red_area_mm2)) ||
      any(tissue_pixels > 0 & tissue_area_mm2 == 0) ||
      any(red_pixels > 0 & red_area_mm2 == 0))
    stop("Calibrated areas exceed finite positive numeric range", call. = FALSE)
  denominator <- ifelse(tissue_area_mm2 > 0, tissue_area_mm2, NA_real_)
  if (any(is.infinite(n_spots / denominator) | is.infinite(n_profiles / denominator)))
    stop("Calibrated densities exceed finite numeric range", call. = FALSE)
  data.frame(n_spots, n_profiles, red_area_mm2, tissue_area_mm2,
             red_area_fraction = red_area_mm2 / denominator,
             spots_per_mm2 = n_spots / denominator,
             profiles_per_mm2 = n_profiles / denominator)
}

#' Aggregate disjoint measurements using summed areas and counts
#'
#' Each `physical_area_id` must occur once. IDs are a safeguard, not proof that
#' two geometries are disjoint; scan overlap must be reconciled upstream.
#' Missing profile counts propagate to the aggregate rather than becoming zero.
#' The default produces descriptive slide-level summaries without requiring
#' animal identities or treatment labels. Unselected metadata are not returned.
#' A slide summary does not establish an independent biological replicate.
#' Optional `qc_flags` are carried into the report without filtering measurements
#' or changing denominators. Compromised input is processed like other input;
#' flags describe interpretation limits rather than validating the counts.
#' @param x Data frame containing grouping columns, `physical_area_id`,
#'   `n_spots`, `n_profiles`, `red_area_mm2`, and `tissue_area_mm2`.
#'   An optional character `qc_flags` column contains semicolon-separated flags;
#'   an empty string means no concern marked in the reviewed material, while `NA`
#'   means quality information is unavailable.
#' @param by Column names defining summaries; defaults to physical slide and organ.
#' @return One data-frame row per group, with area-weighted fractions and densities.
#' @export
aggregate_burden <- function(x, by = c("physical_slide_id", "organ")) {
  reserved <- c("n_measurements", "n_spots", "n_profiles", "red_area_mm2", "tissue_area_mm2",
    "red_area_fraction", "spots_per_mm2", "profiles_per_mm2", "qc_status", "qc_flags",
    "n_flagged_measurements", "n_measurements_without_qc")
  if (any(by %in% reserved)) stop("Grouping columns conflict with generated metrics", call. = FALSE)
  needed <- c(by, "physical_area_id", "n_spots", "n_profiles", "red_area_mm2", "tissue_area_mm2")
  if (!is.data.frame(x) || anyDuplicated(names(x)) || !nrow(x) || !is.character(by) || !length(by) ||
      anyDuplicated(by) || !all(needed %in% names(x)))
    stop("A nonempty measurement table with all required columns is needed", call. = FALSE)
  if (anyNA(x[c(by, "physical_area_id")]) || any(!nzchar(as.character(x$physical_area_id))) ||
      any(vapply(x[by], function(z) any(!nzchar(as.character(z))), logical(1))))
    stop("Grouping and physical-area identifiers must be known", call. = FALSE)
  if (anyDuplicated(x$physical_area_id))
    stop("Duplicate physical_area_id; resolve repeated or overlapping measurements", call. = FALSE)
  for (name in c("n_spots", "red_area_mm2", "tissue_area_mm2"))
    check_nonnegative(x[[name]], name, name == "n_spots")
  check_nonnegative(x$n_profiles[!is.na(x$n_profiles)], "n_profiles", TRUE)
  if ("qc_flags" %in% names(x) && !is.character(x$qc_flags))
    stop("qc_flags must be a character column", call. = FALSE)
  if (any(x$red_area_mm2 > x$tissue_area_mm2) ||
      any(x$red_area_mm2 == 0 & (x$n_spots > 0 | (!is.na(x$n_profiles) & x$n_profiles > 0))))
    stop("Inconsistent tissue area or object counts", call. = FALSE)
  keys <- unique(x[by])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    selected <- Reduce(`&`, lapply(by, function(name) x[[name]] == keys[[name]][i]))
    part <- x[selected, , drop = FALSE]
    area <- sum(part$tissue_area_mm2)
    denominator <- if (area > 0) area else NA_real_
    red <- sum(part$red_area_mm2)
    spots <- .bs_sum_exact_counts(part$n_spots)
    profiles <- .bs_sum_exact_counts(part$n_profiles)
    if (!is.finite(area) || !is.finite(red) ||
        any(is.infinite(c(spots, profiles) / denominator)))
      stop("Pooled measurements exceed finite numeric range", call. = FALSE)
    qc <- if ("qc_flags" %in% names(part)) part$qc_flags else rep(NA_character_, nrow(part))
    flags <- sort(unique(trimws(unlist(strsplit(qc[!is.na(qc)], ";", fixed = TRUE)))))
    flags <- flags[nzchar(flags)]
    qc_status <- if (length(flags)) "flagged_included" else if (anyNA(qc))
      "quality_not_fully_recorded" else "no_concern_marked_in_reviewed_material"
    cbind(keys[i, , drop = FALSE], data.frame(n_measurements = nrow(part),
      n_spots = spots, n_profiles = profiles, tissue_area_mm2 = area,
      red_area_mm2 = red, red_area_fraction = red / denominator,
      spots_per_mm2 = spots / denominator, profiles_per_mm2 = profiles / denominator,
      qc_status = qc_status, qc_flags = paste(flags, collapse = ";"),
      n_flagged_measurements = sum(!is.na(qc) & nzchar(trimws(qc))),
      n_measurements_without_qc = sum(is.na(qc))))
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}
