test_that("anisotropic pixel areas and true zeros are preserved", {
  x <- measure_burden(c(10, 0), c(200, 0), c(10000, 0), .25, .5)
  expect_equal(x$tissue_area_mm2, c(.00125, 0))
  expect_equal(x$red_area_fraction, c(.02, NA_real_))
  expect_equal(x$spots_per_mm2, c(8000, NA_real_))
  expect_true(all(is.na(x$n_profiles)))
  expect_error(measure_burden(1, 0, 0, .25, .25), "without evaluable")
  expect_error(measure_burden(0, 3, 2, .25, .25), "exceed")
  expect_error(measure_burden(0, 0, 2, 0, .25), "positive")
})

test_that("aggregation uses total area and does not average fractions", {
  x <- measure_burden(c(10, 0), c(50, 0), c(100, 900), 1, 1, c(5, 0))
  x$physical_slide_id <- "s1"; x$organ <- "liver"; x$physical_area_id <- c("p1", "p2")
  result <- aggregate_burden(x)
  expect_equal(result$red_area_fraction, .05)
  expect_equal(result$spots_per_mm2, 10000)
  expect_equal(result$n_profiles, 5)
  x$n_profiles[2] <- NA_real_
  expect_true(is.na(aggregate_burden(x)$n_profiles))
  x$physical_area_id[2] <- "p1"
  expect_error(aggregate_burden(x), "Duplicate physical")
})

test_that("slide aggregation requires a slide identity, not an animal key", {
  x <- measure_burden(0, 0, 100, 1, 1)
  x$physical_slide_id <- NA_character_; x$organ <- "liver"; x$physical_area_id <- "a"
  expect_error(aggregate_burden(x), "must be known")
  x$physical_slide_id <- "s1"
  expect_equal(aggregate_burden(x)$n_spots, 0)
})

test_that("distinct slides stay separate and unrelated labels are not emitted", {
  x <- measure_burden(c(2, 1, 3), c(20, 10, 30), c(100, 200, 300), 1, 1)
  x$physical_slide_id <- c("s1", "s2", "s1")
  x$organ <- "liver"
  x$physical_area_id <- c("p1", "p2", "p3")
  x$candidate_id <- "same_filename_prefix"
  x$animal_id <- NA_character_
  x$treatment_group <- "unused_metadata"
  result <- aggregate_burden(x)
  expect_equal(result$physical_slide_id, c("s1", "s2"))
  expect_equal(result$n_spots, c(5, 1))
  expect_equal(result$red_area_fraction, c(.125, .05))
  expect_equal(result$n_measurements, c(2L, 1L))
  expect_false(any(c("candidate_id", "animal_id", "treatment_group") %in% names(result)))
  expect_true(all(is.na(result$n_profiles)))
})

test_that("quality flags retain compromised measurements in counts and area", {
  x <- measure_burden(c(2, 6, 0), c(20, 60, 0), c(100, 300, 100), 1, 1)
  x$physical_slide_id <- "s1"; x$organ <- "liver"
  x$physical_area_id <- c("p1", "p2", "p3")
  x$qc_flags <- c("", "blur;fold", NA_character_)
  result <- aggregate_burden(x)
  expect_equal(result$n_measurements, 3L)
  expect_equal(result$n_spots, 8)
  expect_equal(result$tissue_area_mm2, .0005)
  expect_equal(result$red_area_fraction, .16)
  expect_equal(result$qc_status, "flagged_included")
  expect_equal(result$qc_flags, "blur;fold")
  expect_equal(result$n_flagged_measurements, 1L)
  expect_equal(result$n_measurements_without_qc, 1L)
  x$qc_flags <- rep(NA_character_, 3)
  expect_equal(aggregate_burden(x)$qc_status, "quality_not_fully_recorded")
})
