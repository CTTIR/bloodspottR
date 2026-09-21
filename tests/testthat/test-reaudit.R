test_that("package conditions are catchable and preserve literal user text", {
  expect_error(bs_project(""), class = "bloodspottr_error")
  err <- tryCatch(.bs_abort("Literal {x} and ", "100%"), error = identity)
  expect_s3_class(err, "bloodspottr_error")
  expect_identical(conditionMessage(err), "Literal {x} and 100%")
})

test_that("application upload checks counts before rounding and preserves text IDs", {
  p <- tempfile(fileext = ".csv"); on.exit(unlink(p))
  for (count in c("9007199254740993", "9.007199254740993e15", "1.00000000000000001")) {
    writeLines(c("slide_id,tissue_area_mm2,red_area_mm2,candidate_spots",
                 paste0("001,1,1,", count)), p)
    expect_error(.bs_app_read(p, "input.csv"), "Count")
  }
  writeLines(c("slide_id,tissue_area_mm2,red_area_mm2,candidate_spots",
               "NA,1,1,9007199254740992"), p)
  x <- .bs_app_read(p, "input.csv")
  expect_identical(x$slides$slide_id, "NA")
  expect_equal(x$slides$candidate_spots, 2^53)
  writeLines('{"slides":[{"slide_id":"001","tissue_area_mm2":1,"red_area_mm2":1,"candidate_spots":9007199254740993}]}', p)
  expect_error(.bs_app_read(p, "input.json"), "Count")
})

test_that("application identities, schemas and rates cannot disagree silently", {
  x <- bs_example(); x$slides$slide_id <- rep("wrong", 8)
  expect_error(.bs_app_results(x), "must agree")
  x <- bs_example(); x$slides$physical_slide_id[1] <- " "
  expect_error(.bs_app_results(x), "nonempty")
  x <- bs_example(); x$schema_version <- "99"
  expect_error(.bs_app_results(x), "Unsupported")
  x <- bs_example(); x$slides$red_percent <- 99
  d <- .bs_app_results(x)$slides
  expect_equal(d$red_percent[1:3], c(0, 1, 3))
  expect_true(is.na(d$red_percent[4]))
  x <- bs_example(); x$slides$tissue_area_mm2[1] <- 1e-320
  x$slides$candidate_spots[1] <- x$slides$operational_events[1] <- 1
  expect_error(.bs_app_results(x), "Derived rates")
})

test_that("literal All metadata is filterable and synthetic provenance stays visible", {
  skip_if_not_installed("shiny")
  x <- bs_example(); x$slides$organ[1] <- "All"
  x$slides$stratum[1] <- "All"
  shiny::testServer(.bs_app_server(.bs_app_results(x)), {
    session$setInputs(organ = "", qc = "", search = "")
    expect_equal(nrow(filtered()), 8)
    expect_match(output$status, "DEMONSTRATION")
    expect_match(output$completeness, "6 of 8")
    session$setInputs(organ = "All", qc = "All")
    expect_identical(filtered()$slide_id, "DEMO-001")
    session$setInputs(upload = data.frame(name = "bad.rds", datapath = tempfile()))
    expect_match(output$status, "Previous results retained")
    expect_identical(filtered()$slide_id, "DEMO-001")
  })
})

test_that("saved comparisons survive canonical import and all report formats", {
  x <- bs_example()
  x$slides$support_id <- x$slides$physical_slide_id
  y <- x; y$slides$red_area_mm2[2] <- .3
  x$comparison <- bs_compare(list(first = x, second = y))
  p <- tempfile(); on.exit(unlink(p, recursive = TRUE))
  bs_report(x, p)
  z <- bs_import_legacy(file.path(p, "results.json"))
  expect_equal(z$comparison$red_area_mm2_delta, x$comparison$red_area_mm2_delta)
  expect_identical(z$comparison$physical_slide_id, x$comparison$physical_slide_id)
  expect_true(file.exists(file.path(p, "Comparison.csv")))
  expect_match(paste(readLines(file.path(p, "report.html")), collapse = ""), "Recorded comparison")
  expect_true(any(grepl("  Comparison.csv$", readLines(file.path(p, "SHA256SUMS")))))
})

test_that("canonical CSV preserves a literal NA slide ID", {
  x <- bs_example()$slides[1, ]; x$physical_slide_id <- "NA"
  p <- tempfile(fileext = ".csv"); on.exit(unlink(p))
  write.csv(x, p, row.names = FALSE, na = "")
  expect_identical(bs_import_legacy(p)$slides$physical_slide_id, "NA")
})

test_that("event grouping cannot duplicate generated event columns", {
  x <- data.frame(physical_area_id = "a", physical_slide_id = "s",
    organ = "liver", n_events = 1, red_area_mm2 = .1, tissue_area_mm2 = 1)
  for (name in c("n_events", "events_per_mm2", "relative_positive_area", "relative_positive_area_percent")) {
    x[[name]] <- 1
    expect_error(aggregate_event_burden(x, by = name), "conflict")
  }
})
