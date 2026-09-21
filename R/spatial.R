#' Convert local tissue polygons into a spatial observation window
#'
#' Preserves holes and disconnected tissue. Input coordinates must be local
#' image coordinates with no geographic CRS; the declared unit is explicit.
#' Overlapping polygons are unioned before conversion.
#' @param polygons An `sf` or `sfc` object containing valid polygons only.
#' @param coordinate_unit Either `um` or `mm`; returned coordinates use micrometres.
#' @return A `spatstat.geom` polygonal observation window in micrometres.
#' @export
tissue_window <- function(polygons, coordinate_unit = c("um", "mm")) {
  coordinate_unit <- match.arg(coordinate_unit)
  if (!requireNamespace("sf", quietly = TRUE) || !requireNamespace("spatstat.geom", quietly = TRUE))
    stop("Install sf and spatstat.geom for tissue windows", call. = FALSE)
  if (inherits(polygons, "sf")) polygons <- sf::st_geometry(polygons)
  if (!inherits(polygons, "sfc") || !length(polygons) || !is.na(sf::st_crs(polygons)))
    stop("Supply local image polygons with no CRS and an explicit coordinate unit", call. = FALSE)
  if (any(!sf::st_geometry_type(polygons) %in% c("POLYGON", "MULTIPOLYGON")) ||
      any(sf::st_is_empty(polygons)) || anyNA(sf::st_is_valid(polygons)) || any(!sf::st_is_valid(polygons)))
    stop("Tissue geometry must contain valid nonempty polygons", call. = FALSE)
  united <- sf::st_union(polygons)
  parts <- suppressWarnings(sf::st_cast(united, "POLYGON"))
  multiplier <- if (coordinate_unit == "mm") 1000 else 1
  rings <- list()
  for (polygon in parts) {
    for (i in seq_along(polygon)) {
      coordinates <- polygon[[i]][, 1:2, drop = FALSE] * multiplier
      if (all(coordinates[1, ] == coordinates[nrow(coordinates), ]))
        coordinates <- coordinates[-nrow(coordinates), , drop = FALSE]
      if (nrow(coordinates) < 3 || any(!is.finite(coordinates)))
        stop("Invalid tissue ring coordinates", call. = FALSE)
      next_vertex <- c(2:nrow(coordinates), 1L)
      # Translation prevents cancellation when a small ring has a large origin.
      local <- sweep(coordinates, 2L, coordinates[1L, ], "-")
      signed_area <- sum(local[, 1] * local[next_vertex, 2] -
                           local[next_vertex, 1] * local[, 2]) / 2
      if (signed_area == 0) stop("Zero-area tissue ring", call. = FALSE)
      # Observation windows require anticlockwise exteriors and clockwise holes.
      if ((i == 1 && signed_area < 0) || (i > 1 && signed_area > 0))
        coordinates <- coordinates[nrow(coordinates):1, , drop = FALSE]
      rings[[length(rings) + 1L]] <- list(x = coordinates[, 1], y = coordinates[, 2])
    }
  }
  window <- spatstat.geom::owin(poly = rings)
  spatstat.geom::unitname(window) <- c("micrometre", "micrometres")
  attr(window, "bloodspottR_coordinate_unit") <- "um"
  expected <- as.numeric(sf::st_area(united)) * multiplier^2
  if (!isTRUE(all.equal(spatstat.geom::area.owin(window), expected, tolerance = 1e-8)))
    stop("Tissue area changed during window conversion", call. = FALSE)
  window
}

#' Construct a cell point pattern inside observed tissue
#' @param cells Data frame with unique `cell_id` and finite `x_um`, `y_um` columns.
#' @param window A tissue observation window in micrometres from `tissue_window()`.
#' @return A marked `spatstat.geom` point pattern. Points in holes or outside
#'   tissue, duplicate IDs and duplicate coordinates produce errors.
#' @export
cell_pattern <- function(cells, window) {
  if (!requireNamespace("spatstat.geom", quietly = TRUE))
    stop("Install spatstat.geom for point patterns", call. = FALSE)
  if (!is.data.frame(cells) || !all(c("cell_id", "x_um", "y_um") %in% names(cells)) ||
      !inherits(window, "owin") || !identical(attr(window, "bloodspottR_coordinate_unit"), "um"))
    stop("Cell table and tissue window with verified micrometre units required", call. = FALSE)
  if (!is.numeric(cells$x_um) || !is.numeric(cells$y_um) ||
      any(!is.finite(cells$x_um)) || any(!is.finite(cells$y_um)) ||
      anyNA(cells$cell_id) || any(!nzchar(as.character(cells$cell_id))))
    stop("Cell identifiers and coordinates must be known", call. = FALSE)
  if (anyDuplicated(cells$cell_id) || anyDuplicated(cells[c("x_um", "y_um")]))
    stop("Duplicate cell IDs or coordinates require review", call. = FALSE)
  if (any(!spatstat.geom::inside.owin(cells$x_um, cells$y_um, window)))
    stop("Cells occur outside evaluable tissue or inside excluded holes", call. = FALSE)
  spatstat.geom::ppp(cells$x_um, cells$y_um, window = window,
                    marks = data.frame(cell_id = cells$cell_id))
}
