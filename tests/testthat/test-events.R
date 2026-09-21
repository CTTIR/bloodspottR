test_that("event splitting does not change independent union positive area", {
  result <- measure_event_burden(c(1, 2), c(28, 28), c(64, 64), 2, 3)
  expect_equal(result$n_events, c(1, 2))
  expect_equal(result$relative_positive_area_percent, c(43.75, 43.75))
  expect_equal(result$red_area_mm2, rep(28 * 6 / 1e6, 2))
  expect_false("n_profiles" %in% names(result))
})

test_that("event aggregation retains compromised measurements and weights by area", {
  x <- data.frame(physical_slide_id = c("S1", "S1"), organ = c("liver", "liver"),
    physical_area_id = c("A", "B"), n_events = c(2, 3),
    red_area_mm2 = c(1, 4), tissue_area_mm2 = c(10, 20), qc_flags = c("blur", ""))
  result <- aggregate_event_burden(x)
  expect_equal(result$n_events, 5)
  expect_equal(result$relative_positive_area, 5 / 30)
  expect_equal(result$events_per_mm2, 5 / 30)
  expect_equal(result$qc_status, "flagged_included")
  expect_equal(result$qc_flags, "blur")
  x$physical_area_id[2] <- "A"
  expect_error(aggregate_event_burden(x), "Duplicate physical_area_id")
})

test_that("empty tissue is undefined and inconsistent event inputs are rejected", {
  result <- measure_event_burden(0, 0, 0, .25, .25)
  expect_true(is.na(result$relative_positive_area_percent))
  expect_true(is.na(result$events_per_mm2))
  expect_error(measure_event_burden(1, 0, 10, .25, .25), "supporting positive pixels")
  expect_error(aggregate_event_burden(data.frame(n_spots = 1)), "n_events")
})
