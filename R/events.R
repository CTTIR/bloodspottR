#' Measure positive events and relative positive area
#'
#' An event is an object under the declared segmentation rule and need not be a
#' single erythrocyte. Positive area is the union of red pixels within tissue,
#' supplied independently of event masks. Splitting an event changes its count
#' but must not change these red pixels. Quality flags are handled during
#' aggregation and do not remove tissue or detections.
#' @param n_events Nonnegative whole event counts within tissue.
#' @param red_pixels Nonnegative whole counts of union positive pixels in tissue.
#' @param tissue_pixels Nonnegative whole tissue-pixel counts.
#' @param pixel_width_um,pixel_height_um Pixel dimensions in micrometres.
#' @return A data frame with event counts, calibrated areas, positive area
#'   fraction and percentage, and events per square millimetre. Zero tissue area
#'   yields undefined fractions and densities. This function validates arithmetic,
#'   not assay performance or single-cell identity.
#' @export
#' @examples
#' measure_event_burden(2, 28, 64, 0.25, 0.25)
measure_event_burden <- function(n_events, red_pixels, tissue_pixels,
                                 pixel_width_um, pixel_height_um) {
  result <- measure_burden(n_events, red_pixels, tissue_pixels,
                           pixel_width_um, pixel_height_um)
  data.frame(n_events = result$n_spots,
    red_area_mm2 = result$red_area_mm2,
    tissue_area_mm2 = result$tissue_area_mm2,
    relative_positive_area = result$red_area_fraction,
    relative_positive_area_percent = 100 * result$red_area_fraction,
    events_per_mm2 = result$spots_per_mm2)
}

#' Aggregate event counts and relative positive area by slide
#'
#' Sum counts and union areas from disjoint physical measurement regions. The
#' caller must resolve overlapping scans and event ownership before aggregation.
#' Optional quality flags propagate without excluding compromised measurements.
#' @param x Data frame with grouping columns, physical_area_id, n_events,
#'   red_area_mm2 and tissue_area_mm2. Optional qc_flags are semicolon-separated
#'   character values; NA indicates unavailable quality information.
#' @param by Grouping columns; defaults to physical slide and organ.
#' @return Slide-level sums, area-weighted positive fraction and percentage,
#'   event density, and retained quality flags. No animal/group inference.
#' @export
aggregate_event_burden <- function(x, by = c("physical_slide_id", "organ")) {
  if (!is.data.frame(x) || !"n_events" %in% names(x))
    stop("An event measurement table with n_events is required", call. = FALSE)
  input <- x
  input$n_spots <- input$n_events
  input$n_profiles <- rep(NA_real_, nrow(input))
  result <- aggregate_burden(input, by = by)
  names(result)[names(result) == "n_spots"] <- "n_events"
  names(result)[names(result) == "spots_per_mm2"] <- "events_per_mm2"
  names(result)[names(result) == "red_area_fraction"] <- "relative_positive_area"
  result$relative_positive_area_percent <- 100 * result$relative_positive_area
  result[c("n_profiles", "profiles_per_mm2")] <- NULL
  result
}
