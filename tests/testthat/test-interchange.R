bs_test_interchange <- function() list(
  events = data.frame(event_id = c("e1", "e2"), image_id = "i1", physical_slide_id = "s1",
    x_um = c(1, 2), y_um = c(3, 4), event_type = c("spot_candidate", "bulk_candidate")),
  images = data.frame(image_id = "i1", physical_slide_id = "s1", pixel_width_um = 0.5,
    pixel_height_um = 0.25, width_px = 100L, height_px = 80L))

test_that("exchange rejects broken identity and calibration before backend loading", {
  d <- bs_test_interchange()
  e <- d$events; e$event_id[2] <- "e1"
  expect_error(bs_as_cellspec(e, d$images), "unique within")
  e <- d$events; e$image_id[1] <- "missing"
  expect_error(bs_as_cellspec(e, d$images), "occur in images")
  e <- d$events; e$physical_slide_id[1] <- "wrong"
  expect_error(bs_as_cellspec(e, d$images), "disagrees")
  e <- d$events; e$x_um[1] <- 50
  expect_error(bs_as_cellspec(e, d$images), "extent")
  e <- d$events; e$y_um[1] <- NA_real_
  expect_error(bs_as_cellspec(e, d$images), "finite")
  e <- d$events; e$event_type[1] <- " "
  expect_error(bs_as_cellspec(e, d$images), "nonempty")
  expect_error(bs_as_cellspec(d$events[,-1], d$images), "required columns")
  i <- d$images; i$pixel_height_um <- 0
  expect_error(bs_as_cellspec(d$events, i), "positive")
  i <- d$images; i$width_px <- 10.5
  expect_error(bs_as_cellspec(d$events, i), "integer")
  expect_error(bs_as_cellspec(d$events, rbind(d$images, d$images)), "unique in images")
  expect_error(bs_as_cellspec(d$events, d$images, list(bloodspottR = "override")), "reserved")
  expect_error(bs_as_cellspec(d$events, d$images, list("unnamed")), "named list")
})

test_that("cellspec interchange preserves candidate semantics and anisotropic coordinates", {
  skip_if_not_installed("cellspecR")
  d <- bs_test_interchange()
  x <- bs_as_cellspec(d$events, d$images, list(source = "synthetic test"))
  expect_s3_class(x, "cellspec")
  cells <- cellspecR::cs_cells(x)
  expect_equal(cells$cell_id, d$events$event_id)
  expect_equal(cells$x_px, c(2, 4))
  expect_equal(cells$y_px, c(12, 16))
  expect_equal(cells$object_type, rep("event_candidate", 2))
  expect_equal(cells$classification, d$events$event_type)
  expect_equal(cellspecR::cs_images(x)$pixel_size_y, 0.25)
  expect_equal(dim(cellspecR::cs_measurements(x)), c(2L, 0L))
  expect_match(cellspecR::cs_provenance(x)$bloodspottR$semantics, "not validated")
  zero <- bs_as_cellspec(d$events[FALSE, ], d$images)
  expect_equal(nrow(cellspecR::cs_cells(zero)), 0)
})

test_that("invalid types and provenance are rejected explicitly", {
  d <- bs_test_interchange()
  e <- d$events; e$x_um <- c("1", "2")
  expect_error(bs_as_cellspec(e, d$images), "finite nonnegative")
  e <- d$events; e$event_id <- factor(e$event_id)
  expect_error(bs_as_cellspec(e, d$images), "character identifiers")
  i <- d$images; i$width_px <- Inf
  expect_error(bs_as_cellspec(d$events, i), "finite and positive")
  i <- d$images; i$width_px <- 2^.Machine$integer.max
  expect_error(bs_as_cellspec(d$events, i), "finite and positive")
  i <- d$images; i$width_px <- .Machine$integer.max + 1
  expect_error(bs_as_cellspec(d$events, i), "integer dimensions")
  expect_error(bs_as_cellspec(d$events, d$images, "text"), "named list")
  expect_error(bs_as_cellspec(d$events, d$images, setNames(list(1, 2), c("a", "a"))), "named list")
})

test_that("candidate exchange survives verified cellspec storage", {
  skip_if_not_installed("cellspecR")
  d <- bs_test_interchange()
  x <- bs_as_cellspec(d$events, d$images)
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  cellspecR::cs_write(x, path, format = "tsv.gz")
  y <- cellspecR::cs_read_cellspec(path, verify = TRUE)
  expect_equal(cellspecR::cs_cells(y), cellspecR::cs_cells(x))
  expect_equal(cellspecR::cs_images(y), cellspecR::cs_images(x))
  expect_equal(cellspecR::cs_provenance(y)$bloodspottR, cellspecR::cs_provenance(x)$bloodspottR)
})
