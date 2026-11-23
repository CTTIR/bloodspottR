# Regression tests: malformed inputs raise catchable package conditions
# instead of base R errors.

test_that("results without a slides table fail with a package condition", {
  expect_error(bs_validate(structure(list(slides = NULL), class = "bs_result")),
               "slides data frame", class = "bloodspottr_error")
  expect_error(bs_validate(structure(1, class = "bs_result")),
               "slides data frame", class = "bloodspottr_error")
  expect_error(bs_summarize(structure(list(slides = "a"), class = "bs_result")),
               class = "bloodspottr_error")
  expect_error(bs_validate(list(slides = bs_example()$slides)),
               "Expected bs_result", class = "bloodspottr_error")
})

test_that("unreadable result files fail with a package condition", {
  root <- tempfile("bs-import-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  array_json <- file.path(root, "array.json")
  writeLines("[1, 2, 3]", array_json)
  expect_error(bs_import_legacy(array_json), "result object", class = "bloodspottr_error")
  rows_json <- file.path(root, "rows.json")
  writeLines('[{"physical_slide_id": "a", "rows": 1}]', rows_json)
  expect_error(bs_import_legacy(rows_json), "result object", class = "bloodspottr_error")
  broken_json <- file.path(root, "broken.json")
  writeLines("{bad", broken_json)
  expect_error(bs_import_legacy(broken_json), "could not be parsed", class = "bloodspottr_error")
  empty_csv <- file.path(root, "empty.csv")
  file.create(empty_csv)
  expect_error(bs_import_legacy(empty_csv), "could not be read", class = "bloodspottr_error")
  ragged_csv <- file.path(root, "ragged.csv")
  writeLines(c("physical_slide_id,tissue_area_mm2", "a,1,2,3"), ragged_csv)
  expect_error(bs_import_legacy(ragged_csv), class = "bloodspottr_error")
})

test_that("unknown choices fail with a package condition and keep matching rules", {
  x <- bs_example()
  expect_error(bs_import_legacy(tempfile(fileext = ".csv"), profile = "study"),
               "profile should be one of", class = "bloodspottr_error")
  expect_error(bs_compare(list(a = x, b = x), support = "intersection"),
               "support should be one of", class = "bloodspottr_error")
  expect_error(bs_compare(list(a = x, b = x), support = 1),
               "support should be one of", class = "bloodspottr_error")
  # Partial matching and the first-choice default behave as before.
  expect_identical(nrow(bs_compare(list(a = x, b = x), support = "uni")), nrow(x$slides))
  out <- tempfile("bs-export-")
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  bs_export_results(x, out)
  expect_identical(bs_import_legacy(out, profile = "canon")$profile, "canonical")
})

test_that("grouping by a matrix column is rejected before subsetting", {
  x <- bs_example()
  x$slides$layout <- matrix(seq_len(2L * nrow(x$slides)), ncol = 2L)
  expect_error(bs_summarize(x, strata = "layout"), "atomic vectors without dimensions",
               class = "bloodspottr_error")
  revision <- data.frame(physical_slide_id = x$slides$physical_slide_id)
  revision$block <- matrix("a", nrow(revision), 2L)
  expect_error(bs_summarize(bs_example(), strata = "block", qc_revision = revision),
               "atomic vectors without dimensions", class = "bloodspottr_error")
})

test_that("status reports the import source only for result objects", {
  slides <- bs_example()$slides
  slides$source <- "scanner A"
  expect_null(bs_status(slides)$source)
  out <- tempfile("bs-status-")
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  bs_export_results(bs_example(), out)
  status <- bs_status(bs_import_legacy(out))
  expect_identical(status$source$sha256,
                   digest::digest(file = file.path(out, "results.json"), algo = "sha256"))
})

test_that("all-missing logical profile counts aggregate as unknown", {
  m <- measure_burden(c(10, 4), c(200, 80), c(10000, 5000), 0.25, 0.25)
  m$physical_slide_id <- "s1"
  m$organ <- "liver"
  m$physical_area_id <- c("a1", "a2")
  expected <- aggregate_burden(m)
  m$n_profiles <- NA
  expect_identical(aggregate_burden(m), expected)
  expect_true(is.na(expected$n_profiles))
  m$n_profiles <- c(TRUE, NA)
  expect_error(aggregate_burden(m), "n_profiles", class = "bloodspottr_error")
  m$n_profiles <- matrix(NA, 2L, 1L)
  expect_error(aggregate_burden(m), "n_profiles", class = "bloodspottr_error")
})

test_that("missing application files are reported by the package", {
  skip_if_not_installed("shiny")
  missing <- file.path(tempdir(), "bs-no-such-result.json")
  expect_error(bs_app(missing, launch = FALSE), "Result file does not exist",
               class = "bloodspottr_error")
  expect_error(bs_app(NA_character_, launch = FALSE), "Result file does not exist",
               class = "bloodspottr_error")
  expect_error(bs_app(bs_example(), launch = FALSE,
                      images = file.path(tempdir(), "bs-no-such-manifest.csv")),
               "Image manifest file does not exist", class = "bloodspottr_error")
  expect_error(bs_app(bs_example(), launch = FALSE, images = tempdir()),
               "Image manifest file does not exist", class = "bloodspottr_error")
})
