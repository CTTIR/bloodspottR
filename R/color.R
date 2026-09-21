#' Extract the study's three relative optical-density color features
#'
#' Computes channel OD as `-log((channel + pseudocount) /
#' (white + pseudocount))`. Output channels are green OD minus red OD,
#' blue OD minus red OD, and mean channel OD, in that order. With the default
#' settings this is the three-feature definition used by the study's RGB/OD
#' networks, evaluated in double precision. The first two features are log
#' red-to-green and red-to-blue chromaticity ratios. These are relative digital
#' image features, not calibrated chromophore concentrations or fitted labels.
#'
#' No rescaling, clipping, white estimation or statistical fitting is performed.
#' `white` is an explicit common channel reference in the input's intensity units.
#' All pixels must be finite and within `[0, white]`; masks and missing pixels
#' must be handled explicitly before calling this function.
#' @param rgb Numeric matrix with three columns, or array with dimensions
#'   height by width by three channels. Channels must be ordered red, green, blue.
#' @param white Finite positive common channel white reference, default 255.
#' @param pseudocount Finite positive offset in input intensity units, default 1.
#' @return Numeric object with the same dimensions as `rgb`, with its final
#'   dimension named `od_g_minus_r`, `od_b_minus_r`, `od_mean`.
#' @export
#' @examples
#' bs_color_features(rbind(c(255, 255, 255), c(255, 127, 63)))
bs_color_features <- function(rgb, white = 255, pseudocount = 1) {
  dims <- dim(rgb)
  if (!is.numeric(rgb) || !length(dims) %in% c(2L, 3L) ||
      utils::tail(dims, 1L) != 3L || any(dims < 1L))
    .bs_abort("rgb must be a nonempty numeric RGB matrix or height-by-width-by-3 array", call. = FALSE)
  for (value in list(white, pseudocount))
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value <= 0)
      .bs_abort("white and pseudocount must be finite positive scalars", call. = FALSE)
  if (!is.finite(white + pseudocount)) .bs_abort("white plus pseudocount must be finite", call. = FALSE)
  if (any(!is.finite(rgb) | rgb < 0 | rgb > white))
    .bs_abort("RGB values must be finite and between zero and white", call. = FALSE)
  od <- matrix(log(white + pseudocount) - log(rgb + pseudocount), ncol = 3L)
  features <- cbind(od[, 2L] - od[, 1L], od[, 3L] - od[, 1L], rowMeans(od))
  dim(features) <- dims
  labels <- dimnames(rgb)
  if (is.null(labels)) labels <- vector("list", length(dims))
  labels[[length(dims)]] <- c("od_g_minus_r", "od_b_minus_r", "od_mean")
  dimnames(features) <- labels
  features
}

#' Compare a calibrated profile's diameter with an explicit reference
#'
#' Measures one component from its filled-profile boundary coordinates. The
#' maximum distance between any two supplied points equals the diameter of their
#' convex hull. Rotating calipers evaluate that diameter without allocating an
#' all-pairs distance matrix. Duplicated coordinates and collinear profiles are
#' supported. Horizontal and vertical extents use the supplied image axes.
#'
#' Coordinates must describe the outer profile, not only its annotation centre,
#' and must already be calibrated in micrometres; apply separate x/y pixel
#' calibration before use. Sparse boundary sampling can underestimate the true
#' extent. The explicit upper reference requires slide-specific calibration.
#' `single_compatible` only means that measured diameter does not exceed that
#' reference. `exceeds_reference` is a review flag, never proof of multiple cells
#' or an estimated cell count. Section plane, deformation, overlap and
#' segmentation error remain unresolved by this measurement.
#' @param coordinates_um Numeric two-column matrix or data frame, x then y,
#'   describing one component in micrometres. At least one point is required.
#' @param upper_diameter_um Finite positive upper single-profile reference in
#'   micrometres. There is no universal default.
#' @return One-row data frame with maximum diameter, x/y extents, reference,
#'   review flag and number of distinct supplied points. Equality to the upper
#'   reference is classified as single-compatible.
#' @export
#' @examples
#' bs_profile_extent(rbind(c(0, 0), c(3, 0), c(3, 4), c(0, 4)), 5)
bs_profile_extent <- function(coordinates_um, upper_diameter_um) {
  if (is.data.frame(coordinates_um)) {
    if (!all(vapply(coordinates_um, is.numeric, logical(1))))
      .bs_abort("coordinates_um must have two numeric columns", call. = FALSE)
    coordinates_um <- as.matrix(coordinates_um)
  }
  if (!is.matrix(coordinates_um) || !is.numeric(coordinates_um) || ncol(coordinates_um) != 2L ||
      !nrow(coordinates_um) || any(!is.finite(coordinates_um)))
    .bs_abort("coordinates_um must be a nonempty finite two-column numeric matrix", call. = FALSE)
  if (!is.numeric(upper_diameter_um) || length(upper_diameter_um) != 1L ||
      !is.finite(upper_diameter_um) || upper_diameter_um <= 0)
    .bs_abort("upper_diameter_um must be a finite positive scalar", call. = FALSE)
  points <- unique(coordinates_um)
  extents <- apply(points, 2L, function(v) max(v) - min(v))
  if (any(!is.finite(extents))) .bs_abort("Coordinate extent exceeds numerical range", call. = FALSE)
  scale <- max(extents)
  diameter <- 0
  if (scale > 0) {
    # Normalize before cross products and squared distances to avoid overflow.
    centered <- sweep(points, 2L, apply(points, 2L, min), "-") / scale
    hull <- centered[grDevices::chull(centered), , drop = FALSE]
    n <- nrow(hull)
    squared <- function(i, j) sum((hull[i, ] - hull[j, ])^2)
    if (n <= 2L) diameter <- sqrt(squared(1L, n)) * scale else {
      following <- function(i) if (i == n) 1L else i + 1L
      area <- function(i, next_i, j) {
        edge <- hull[next_i, ] - hull[i, ]; point <- hull[j, ] - hull[i, ]
        abs(edge[1L] * point[2L] - edge[2L] * point[1L])
      }
      j <- 2L; best <- 0
      for (i in seq_len(n)) {
        ni <- following(i)
        while (area(i, ni, following(j)) > area(i, ni, j)) j <- following(j)
        best <- max(best, squared(i, j), squared(ni, j))
        if (area(i, ni, following(j)) == area(i, ni, j))
          best <- max(best, squared(i, following(j)), squared(ni, following(j)))
      }
      diameter <- sqrt(best) * scale
    }
  }
  if (!is.finite(diameter)) .bs_abort("Profile diameter exceeds numerical range", call. = FALSE)
  data.frame(max_diameter_um = diameter, x_extent_um = extents[1L],
    y_extent_um = extents[2L], upper_diameter_um = upper_diameter_um,
    flag = if (diameter <= upper_diameter_um) "single_compatible" else "exceeds_reference",
    n_unique_points = nrow(points), row.names = NULL)
}
