#' Deterministic demonstration results
#'
#' Synthetic measurements for examples and application exploration, never study
#' data or evidence of model performance.
#' @return A result list with slides, groups and provenance.
#' @export
bs_example <- function() {
  slides <- data.frame(physical_slide_id = sprintf("DEMO-%03d", 1:8),
    organ = rep(c("Liver", "Spleen"), each = 4),
    tissue_area_mm2 = c(10, 20, 15, 0, 12, 18, 25, 16),
    red_area_mm2 = c(0, .2, .45, 0, 1.2, 2.7, 1, NA),
    candidate_spots = c(0, 110, 240, 0, 1200, 2700, 800, NA),
    stratum = rep(c("No substantial finding", "Structure only", "Raster only", "Uncertain"), 2),
    stringsAsFactors = FALSE)
  slides$unresolved_bulk_events <- c(0, 1, 2, 0, 4, 6, 3, NA)
  slides$operational_events <- slides$candidate_spots + slides$unresolved_bulk_events
  structure(list(schema_version = "1.0", slides = slides, groups = data.frame(),
    provenance = list(source = "Deterministic synthetic demonstration", biological_validation = "None")),
    class = c("bs_result", "list"))
}
