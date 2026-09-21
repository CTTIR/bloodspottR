#' Exchange calibrated event candidates through cellspecR
#'
#' Preserves candidate identity and class without asserting that an event is one
#' cell. No intensity features, channel measurements or cell areas are invented.
#' Coordinates must already be in the image frame in micrometres. A physical
#' slide is mapped to cellspec's sample_id; no animal identity is inferred.
#' @param events Data frame with event_id, image_id, physical_slide_id, x_um,
#'   y_um and event_type. Event IDs must be unique within an image.
#' @param images Data frame with image_id, physical_slide_id, pixel_width_um,
#'   pixel_height_um, width_px and height_px. Every referenced image needs an
#'   explicit positive calibration; anisotropic pixels are supported.
#' @param provenance Named list of additional provenance. The bloodspottR entry
#'   is reserved and records the candidate semantics and mapping.
#' @return A validated cellspec object. Requires the optional cellspecR package.
#' @export
bs_as_cellspec <- function(events, images, provenance = list()) {
  components <- list(events = events, images = images)
  required <- list(events = c("event_id", "image_id", "physical_slide_id", "x_um", "y_um", "event_type"),
    images = c("image_id", "physical_slide_id", "pixel_width_um", "pixel_height_um", "width_px", "height_px"))
  for (nm in names(components)) {
    d <- components[[nm]]
    if (!is.data.frame(d) || anyDuplicated(names(d)) || !all(required[[nm]] %in% names(d)))
      stop(nm, " must contain unique named required columns: ", paste(required[[nm]], collapse = ", "), call. = FALSE)
    ids <- intersect(c("event_id", "image_id", "physical_slide_id", "event_type"), required[[nm]])
    for (id in ids) if (!is.character(d[[id]]) || anyNA(d[[id]]) || any(!nzchar(trimws(d[[id]]))))
      stop(nm, "$", id, " must contain nonempty character identifiers", call. = FALSE)
  }
  if (anyDuplicated(images$image_id)) stop("image_id must be unique in images", call. = FALSE)
  if (anyDuplicated(events[c("image_id", "event_id")])) stop("event_id must be unique within image", call. = FALSE)
  for (nm in c("pixel_width_um", "pixel_height_um", "width_px", "height_px")) {
    v <- images[[nm]]
    if (!is.numeric(v) || any(!is.finite(v) | v <= 0)) stop(nm, " must be finite and positive", call. = FALSE)
  }
  for (nm in c("width_px", "height_px")) if (any(images[[nm]] != floor(images[[nm]]) | images[[nm]] > .Machine$integer.max))
    stop(nm, " must be positive integer dimensions", call. = FALSE)
  idx <- match(events$image_id, images$image_id)
  if (anyNA(idx)) stop("Every event image_id must occur in images", call. = FALSE)
  if (any(events$physical_slide_id != images$physical_slide_id[idx])) stop("physical_slide_id disagrees between events and images", call. = FALSE)
  for (nm in c("x_um", "y_um")) if (!is.numeric(events[[nm]]) || any(!is.finite(events[[nm]]) | events[[nm]] < 0))
    stop(nm, " must contain finite nonnegative coordinates", call. = FALSE)
  if (any(events$x_um >= images$width_px[idx] * images$pixel_width_um[idx] |
          events$y_um >= images$height_px[idx] * images$pixel_height_um[idx]))
    stop("Event coordinates exceed the calibrated image extent", call. = FALSE)
  if (!is.list(provenance) || (length(provenance) && (is.null(names(provenance)) || anyNA(names(provenance)) || any(!nzchar(names(provenance))) || anyDuplicated(names(provenance)))) || "bloodspottR" %in% names(provenance))
    stop("provenance must be a uniquely named list without reserved bloodspottR entry", call. = FALSE)
  if (!requireNamespace("cellspecR", quietly = TRUE)) stop("Install optional package cellspecR to exchange event candidates", call. = FALSE)
  cells <- data.frame(cell_id = events$event_id, image_id = events$image_id,
    sample_id = events$physical_slide_id, x = events$x_um, y = events$y_um,
    x_px = events$x_um / images$pixel_width_um[idx], y_px = events$y_um / images$pixel_height_um[idx],
    object_type = rep("event_candidate", nrow(events)), classification = events$event_type)
  metadata <- data.frame(image_id = images$image_id, sample_id = images$physical_slide_id,
    pixel_size = images$pixel_width_um, pixel_size_y = images$pixel_height_um,
    width_px = as.integer(images$width_px), height_px = as.integer(images$height_px))
  provenance$bloodspottR <- list(semantics = "exploratory event candidates; not validated cell counts",
    coordinate_unit = "um", sample_id_source = "physical_slide_id", cell_id_source = "event_id")
  cellspecR::cs_new(cells = cells, images = metadata, provenance = provenance)
}
