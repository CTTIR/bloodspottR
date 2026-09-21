test_that("summary grouping cannot overwrite identity with generated metrics", {
  x <- bs_example()$slides
  x$n_slides <- rep("metadata", nrow(x))
  expect_error(bs_summarize(x, "n_slides"), "distinct metadata")
  x$slide_ids <- x$physical_slide_id
  expect_error(bs_summarize(x, "slide_ids"), "distinct metadata")
  x <- bs_example()$slides
  names(x)[2] <- "physical_slide_id"
  expect_error(bs_validate(x), "Duplicate measurement")
})

test_that("worker receipts retain the exact request and reject lossy settings", {
  skip_if_not_installed("processx")
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  input <- file.path(root, "image.txt"); writeLines("synthetic", input)
  script <- file.path(root, "worker.R")
  writeLines(c('a <- commandArgs(TRUE)',
    'file.copy(a[1], file.path(a[2], "request-copy.json"))',
    'jsonlite::write_json(list(schema_version=1L,status="complete",artifacts=list("request-copy.json")), file.path(a[2],"result.json"), auto_unbox=TRUE)'), script)
  backend <- bs_backend(file.path(R.home("bin"), "Rscript"), script, version = "audit-worker-v1")
  settings <- list(seed = 42, features = c("RGB", "OD"), optimizer = list(rate = .001), optional = NULL)
  out <- file.path(root, "job")
  job <- bs_train(c(image = input), backend, out, parameters = settings)
  expect_equal(job$request$parameters, settings)
  expect_equal(job$request$inputs$image, normalizePath(input))
  expect_identical(job$request_sha256, digest::digest(file = file.path(out, "request-copy.json"), algo = "sha256"))
  saved <- jsonlite::read_json(file.path(out, "job-receipt.json"), simplifyVector = TRUE)
  expect_equal(saved$request$parameters$optimizer$rate, .001)
  for (invalid in list(Inf, NaN, NA_real_, 1 + 2i, as.Date("2026-01-01"), NA_character_, environment())) {
    expect_error(bs_train(c(image = input), backend, file.path(root, "bad"),
      parameters = list(nested = list(value = invalid))), "JSON-compatible")
    expect_false(file.exists(file.path(root, "bad")))
  }
  expect_error(bs_train(c(image = input), backend, file.path(root, "bad"),
    parameters = list(nested = setNames(list(1, 2), c("x", "x")))), "JSON-compatible")
  writeLines(c('a <- commandArgs(TRUE)',
    'writeLines("changed",a[1])', 'writeLines("artifact",file.path(a[2],"data.txt"))',
    'jsonlite::write_json(list(schema_version=1L,status="complete",artifacts=list("data.txt")),file.path(a[2],"result.json"),auto_unbox=TRUE)'), script)
  expect_error(bs_train(c(image = input), backend, file.path(root, "changed")), "request manifest")
  expect_false(file.exists(file.path(root, "changed")))
})

test_that("zero-origin anisotropic exchange uses independent axes and image identities", {
  skip_if_not_installed("cellspecR")
  images <- data.frame(image_id = c("a", "b"), physical_slide_id = c("s1", "s2"),
    pixel_width_um = c(.25, .5), pixel_height_um = c(.5, .25), width_px = c(20, 10), height_px = c(10, 20))
  events <- data.frame(event_id = c("001", "001"), image_id = c("b", "a"),
    physical_slide_id = c("s2", "s1"), x_um = c(0, 4.75), y_um = c(.125, 4.5),
    event_type = "candidate")
  x <- bs_as_cellspec(events, images)
  cells <- cellspecR::cs_cells(x)
  expect_equal(cells$x_px, c(0, 19))
  expect_equal(cells$y_px, c(.5, 9))
  expect_equal(cells$cell_id, c("001", "001"))
  expect_equal(cells$sample_id, c("s2", "s1"))
  events$y_um[1] <- 5
  expect_error(bs_as_cellspec(events, images), "extent")
})

test_that("the shipped demonstration roundtrips through a canonical delivery", {
  x <- bs_example(); out <- tempfile(); on.exit(unlink(out, recursive = TRUE))
  bs_export_results(x, out)
  y <- bs_import_legacy(out)
  expect_equal(y$slides$physical_slide_id, x$slides$physical_slide_id)
  expect_equal(y$slides$candidate_spots, x$slides$candidate_spots)
  expect_equal(bs_summarize(y), bs_summarize(x))
})

test_that("translated polygons, overlap and boundary cells preserve tissue support", {
  skip_if_not_installed("sf"); skip_if_not_installed("spatstat.geom")
  square <- function(x, y, size) rbind(c(x,y), c(x+size,y), c(x+size,y+size), c(x,y+size), c(x,y))
  translated <- sf::st_sfc(sf::st_polygon(list(square(1e8, 1e8, 1))))
  w <- tissue_window(sf::st_sf(id = "large-origin", geometry = translated))
  expect_equal(spatstat.geom::area.owin(w), 1)
  expect_true(spatstat.geom::inside.owin(1e8 + .5, 1e8 + .5, w))
  overlapping <- sf::st_sfc(sf::st_polygon(list(square(0, 0, 10))),
                            sf::st_polygon(list(square(5, 0, 10))))
  w <- tissue_window(overlapping)
  expect_equal(spatstat.geom::area.owin(w), 150)
  points <- data.frame(cell_id = "edge", x_um = 0, y_um = 5)
  expect_equal(spatstat.geom::npoints(cell_pattern(points, w)), 1)
  expect_equal(spatstat.geom::npoints(cell_pattern(points[FALSE, ], w)), 0)
  expect_error(cell_pattern(rbind(points, points), w), "Duplicate")
  points$x_um <- NA_real_; expect_error(cell_pattern(points, w), "known")
  points$x_um <- "a"; expect_error(cell_pattern(points, w), "known")
  expect_error(cell_pattern(data.frame(), w), "verified micrometre")
  unverified <- spatstat.geom::owin(c(0,1), c(0,1))
  expect_error(cell_pattern(data.frame(cell_id="a", x_um=.5, y_um=.5), unverified), "verified micrometre")
  expect_error(tissue_window(sf::st_sfc(sf::st_point(c(0, 0)))), "valid nonempty polygons")
  expect_error(tissue_window(sf::st_sfc(sf::st_polygon())), "valid nonempty polygons")
  bowtie <- sf::st_sfc(sf::st_polygon(list(rbind(c(0,0), c(1,1), c(0,1), c(1,0), c(0,0)))))
  expect_error(tissue_window(bowtie), "valid nonempty polygons")
})

test_that("burden helpers reject unsupported arithmetic and malformed identities", {
  expect_error(measure_burden(1, -1, 10, 1, 1), "nonnegative")
  expect_error(measure_burden(.5, 1, 10, 1, 1), "whole")
  expect_error(measure_burden(1:2, 1, 10, 1, 1), "equal")
  expect_error(measure_burden(1, 1, 10, 1, 1, n_profiles = c(1,2)), "incompatible")
  x <- data.frame(physical_slide_id = "s", organ = "liver", physical_area_id = "a", n_spots = 1,
    n_profiles = NA_real_, red_area_mm2 = 1, tissue_area_mm2 = 2)
  x$physical_area_id <- NA_character_; expect_error(aggregate_burden(x), "identifiers")
  x$physical_area_id <- "a"; x$qc_flags <- TRUE
  expect_error(aggregate_burden(x), "character")
  x$qc_flags <- ""; x$red_area_mm2 <- 3
  expect_error(aggregate_burden(x), "Inconsistent")
})

test_that("legacy burden APIs enforce the same finite and exact-count boundaries", {
  expect_error(measure_event_burden(2^53 + 2, 2^53 + 2, 2^53 + 2, 1, 1), "2\\^53")
  expect_error(measure_burden(1, 1, 10, 1e200, 1e200), "Calibrated areas")
  expect_error(measure_burden(1, 1, 10, 1e-200, 1e-200), "Calibrated areas")
  expect_error(measure_event_burden(1, 1, 1, 1e-160, 1e-155), "Calibrated densities")
  x <- data.frame(physical_slide_id = "s", organ = "liver", physical_area_id = c("a", "b"),
    n_events = c(2^53, 1), red_area_mm2 = c(1, 1), tissue_area_mm2 = c(2, 2))
  expect_error(aggregate_event_burden(x), "Pooled counts")
  x$n_events <- c(1, 1); x$tissue_area_mm2 <- c(1e308, 1e308)
  expect_error(aggregate_event_burden(x), "Pooled measurements")
  x$tissue_area_mm2 <- x$red_area_mm2 <- c(1e-320, 1e-320)
  expect_error(aggregate_event_burden(x), "Pooled measurements")
})

test_that("CSV counts are checked before floating-point rounding", {
  x <- bs_example()$slides[1, ]
  path <- tempfile(fileext = ".csv"); on.exit(unlink(path))
  for (value in c("9007199254740993", "9.007199254740993e15", "1.00000000000000001", "1e16", "1e-1")) {
    x$candidate_spots <- x$operational_events <- value
    write.csv(x, path, row.names = FALSE)
    expect_error(bs_import_legacy(path), "Count")
  }
  x$tissue_area_mm2 <- 1; x$candidate_spots <- x$operational_events <- "9.007199254740992e15"
  write.csv(x, path, row.names = FALSE)
  expect_equal(bs_import_legacy(path)$slides$candidate_spots, 2^53)
  x$candidate_spots <- x$operational_events <- "001.0e1"
  write.csv(x, path, row.names = FALSE)
  expect_equal(bs_import_legacy(path)$slides$candidate_spots, 10)
  for (value in c("0010.0", "1e1")) {
    x$candidate_spots <- x$operational_events <- value
    write.csv(x, path, row.names = FALSE)
    expect_equal(bs_import_legacy(path)$slides$candidate_spots, 10)
  }
  x$red_area_mm2 <- "unparseable"
  write.csv(x, path, row.names = FALSE)
  expect_error(bs_import_legacy(path), "Invalid numeric measurement")
  x$physical_area_id <- "a"; x$n_spots <- 1; x$n_profiles <- NA_real_
  x$n_measurements <- "group label"
  expect_error(aggregate_burden(x, by = "n_measurements"), "conflict")
})
