test_that("application validates data before constructing a session", {
  skip_if_not_installed("shiny")
  x <- bs_example()
  expect_s3_class(bs_app(x, launch = FALSE), "shiny.appobj")
  expect_s3_class(bs_app(launch = FALSE), "shiny.appobj")
  expect_error(bs_app(x, launch = NA), "TRUE or FALSE")
  x$slides$red_area_mm2[1] <- 100
  expect_error(bs_app(x, launch = FALSE), "exceed")
  x <- bs_example(); x$slides$physical_slide_id[2] <- x$slides$physical_slide_id[1]
  expect_error(bs_app(x, launch = FALSE), "unique")
  x <- bs_example(); x$slides$candidate_spots[1] <- .5
  expect_error(bs_app(x, launch = FALSE), "whole counts")
})

test_that("pooled metrics never average percentages or discard missing values", {
  d <- data.frame(tissue_area_mm2 = c(1, 9), red_area_mm2 = c(1, 0), candidate_spots = c(10, 0))
  m <- .bs_app_metrics(d)
  expect_equal(unname(m["red_percent"]), 10)
  expect_equal(unname(m["spots_per_mm2"]), 1)
  d$red_area_mm2[2] <- NA_real_
  expect_true(is.na(.bs_app_metrics(d)["red_percent"]))
  d$tissue_area_mm2 <- 0
  expect_true(is.na(.bs_app_metrics(d)["spots_per_mm2"]))
})

test_that("session filters only views and failed upload preserves existing data", {
  skip_if_not_installed("shiny")
  x <- .bs_app_results(bs_example())
  shiny::testServer(.bs_app_server(x), {
    session$setInputs(organ = "Liver", qc = "All", search = "")
    expect_equal(nrow(filtered()), 4L)
    session$setInputs(qc = "Structure only")
    expect_equal(filtered()$slide_id, "DEMO-002")
    session$setInputs(search = "[invalid regex")
    expect_equal(nrow(filtered()), 0L)
    session$setInputs(upload = data.frame(name = "evil.rds", datapath = tempfile()))
    expect_match(output$status, "Import failed")
    expect_equal(nrow(current()$slides), 8L)
    session$setInputs(demo = 1)
    expect_match(output$status, "DEMONSTRATION")
    expect_null(error())
  })
  expect_identical(x$slides, .bs_app_results(bs_example())$slides)
})

test_that("uploads preserve IDs and reject invalid numeric inputs", {
  p <- tempfile(fileext = ".csv")
  writeLines(c("slide_id,tissue_area_mm2,red_area_mm2,candidate_spots", "001,10,1,25"), p)
  expect_identical(.bs_app_read(p, "result.csv")$slides$slide_id, "001")
  writeLines(c("slide_id,tissue_area_mm2,red_area_mm2,candidate_spots", "001,oops,1,25"), p)
  expect_error(.bs_app_read(p, "result.csv"), "Invalid numeric")
  expect_error(.bs_app_read(p, "result.rds"), "CSV or JSON")
  expect_error(.bs_app_results(list()), "slides data frame")
  expect_error(.bs_app_results(list(slides = data.frame(a = 1))), "Missing slide columns")
  unlink(p)
})

test_that("CSV exports protect text fields without changing numeric data", {
  p <- tempfile(fileext = ".csv")
  d <- data.frame(slide_id = c("=1+1", "normal"), value = c(2, NA))
  .bs_app_csv(d, p)
  roundtrip <- utils::read.csv(p)
  expect_identical(roundtrip$slide_id, c("'=1+1", "normal"))
  expect_equal(roundtrip$value, c(2, NA))
  unlink(p)
})

test_that("UI exposes accessible navigation, explicit demo and provenance", {
  skip_if_not_installed("shiny")
  html <- as.character(.bs_app_ui())
  expect_match(html, "Skip to results")
  expect_match(html, "Load demonstration")
  expect_match(html, "aria-live")
  expect_match(html, "Methods &amp; provenance")
  expect_false(grepl("https://", html, fixed = TRUE))
})

test_that("session renders metrics, tables, provenance and CSV download", {
  skip_if_not_installed("shiny")
  x <- .bs_app_results(bs_example())
  x$comparison <- data.frame(slide_id = x$slides$slide_id, delta = seq_len(8))
  shiny::testServer(.bs_app_server(x), {
    session$setInputs(organ = "Liver", qc = "All", search = "")
    expect_match(output$metrics$html, "Tissue area")
    expect_match(output$slides, "DEMO-001")
    expect_match(output$comparison, "DEMO-001")
    expect_match(output$provenance, "synthetic demonstration")
    expect_type(output$scatter, "list")
    path <- output$download
    downloaded <- utils::read.csv(path)
    expect_equal(nrow(downloaded), 4L)
    expect_identical(downloaded$slide_id, x$slides$slide_id[1:4])
  })
})

test_that("empty session and missing provenance remain explicit", {
  skip_if_not_installed("shiny")
  shiny::testServer(.bs_app_server(NULL), {
    expect_match(output$status, "No results")
    expect_match(output$metrics$html, "Import your result")
    session$setInputs(upload = data.frame(name = "bad.rds", datapath = tempfile()))
    expect_match(output$status, "No results loaded")
  })
  x <- .bs_app_results(bs_example()); x$provenance <- NULL
  shiny::testServer(.bs_app_server(x), {
    expect_match(output$provenance, "No provenance")
  })
})

test_that("JSON and missing metadata use explicit input contracts", {
  skip_if_not_installed("jsonlite")
  p <- tempfile(fileext = ".json")
  jsonlite::write_json(bs_example(), p, dataframe = "rows", na = "null", auto_unbox = TRUE)
  imported <- .bs_app_read(p, "result.json")
  expect_equal(nrow(imported$slides), 8L)
  expect_true(is.na(imported$slides$red_area_mm2[8]))
  x <- bs_example(); x$slides$organ <- NULL; x$slides$stratum <- NULL
  d <- .bs_app_results(x)$slides
  expect_true(all(d$organ == "Unspecified"))
  expect_true(all(d$qc_stratum == "Unspecified"))
  x$slides$tissue_area_mm2[1] <- NaN
  expect_error(.bs_app_results(x), "finite numbers")
  x <- bs_example(); x$slides$organ[1] <- NA
  expect_equal(.bs_app_results(x)$slides$organ[1], "Unspecified")
  unlink(p)
})

test_that("uploads cannot read arbitrary server paths", {
  expect_error(.bs_app_read(file.path(getwd(), "DESCRIPTION"), "data.csv"), "session temporary")
})

test_that("nested metadata exports and all-null JSON measurements are supported", {
  skip_if_not_installed("jsonlite")
  x <- bs_example()
  x$slides$red_area_mm2 <- rep(NA_real_, nrow(x$slides))
  x$slides$qc <- I(lapply(seq_len(nrow(x$slides)), function(i) list(note = "recorded")))
  p <- tempfile(fileext = ".json")
  jsonlite::write_json(x, p, dataframe = "rows", auto_unbox = TRUE, na = "null")
  imported <- .bs_app_read(p, "input.json")
  expect_type(imported$slides$red_area_mm2, "double")
  out <- tempfile(fileext = ".csv")
  expect_silent(.bs_app_csv(imported$slides, out))
  expect_match(utils::read.csv(out)$qc[1], "recorded")
  unlink(c(p, out))
})

test_that("application pooled arithmetic rejects overflow and optional count conflicts", {
  d <- data.frame(tissue_area_mm2 = 1e307, red_area_mm2 = 1e307, candidate_spots = 1)
  expect_equal(unname(.bs_app_metrics(d)["red_percent"]), 100)
  d <- data.frame(tissue_area_mm2 = c(1, 1), red_area_mm2 = c(0, 0), candidate_spots = c(2^53, 1))
  expect_error(.bs_app_metrics(d), "exact double precision")
  x <- bs_example(); x$slides$operational_events[1] <- 3
  expect_error(.bs_app_results(x), "equal spots plus bulk")
})
