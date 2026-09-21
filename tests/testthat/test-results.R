result_fixture <- function() data.frame(physical_slide_id = c("001", "002", "003"),
  organ = c("liver", "liver", NA_character_), support_id = c("a", "b", "c"),
  tissue_area_mm2 = c(1, 9, 0), red_area_mm2 = c(.2, .3, 0),
  candidate_spots = c(2, 3, 0), unresolved_bulk_events = c(1, 0, 0),
  operational_events = c(3, 3, 0))

test_that("canonical results preserve identifiers and pool numerators", {
  x <- result_fixture(); p <- tempfile(fileext = ".csv")
  on.exit(unlink(p)); write.csv(x, p, row.names = FALSE)
  z <- bs_import_legacy(p, "canonical")
  expect_s3_class(z, "bs_result")
  expect_identical(z$slides$physical_slide_id, x$physical_slide_id)
  expect_equal(nchar(z$source$sha256), 64)
  expect_true(bs_validate(z)); expect_equal(bs_status(z)$slides, 3)
  g <- bs_summarize(z, "organ")
  expect_equal(g$red_percent[1], 5)
  expect_equal(g$spots_per_mm2[1], .5)
  expect_true(is.na(g$red_percent[2]))
  expect_equal(g$slide_ids[[1]], c("001", "002"))
  expect_equal(bs_summarize(x)$n_slides, 3)
  x$candidate_spots[1] <- NA_real_; x$operational_events[1] <- NA_real_
  expect_true(is.na(bs_summarize(x)$candidate_spots))
})

test_that("legacy CSV and saved JSON remain portable and unmodified", {
  skip_if_not_installed("jsonlite")
  dir <- tempfile(); dir.create(dir); on.exit(unlink(dir, recursive = TRUE))
  x <- result_fixture(); names(x)[names(x) == "tissue_area_mm2"] <- "tissue_mm2"
  names(x)[names(x) == "red_area_mm2"] <- "red_mm2"
  p <- file.path(dir, "slides.csv"); write.csv(x, p, row.names = FALSE)
  old <- tools::md5sum(p)
  z <- bs_import_legacy(dir)
  expect_equal(z$slides$tissue_area_mm2, x$tissue_mm2)
  expect_identical(tools::md5sum(p), old)
  j <- file.path(dir, "analysis.json")
  jsonlite::write_json(list(rows = x, groups = data.frame(n = 3), note = "saved"), j, auto_unbox = TRUE)
  expect_error(bs_import_legacy(dir), "exactly one")
  expect_equal(bs_import_legacy(j)$provenance$note, "saved")
  expect_equal(bs_import_legacy(j)$groups$n, 3)
  expect_error(bs_import_legacy(p, "canonical"), "columns")
  expect_error(bs_import_legacy(file.path(dir, "absent")), "does not exist")
  q <- file.path(dir, "bad.txt"); writeLines("x", q)
  expect_error(bs_import_legacy(q), "Only CSV")
  jsonlite::write_json(list(rows = list()), j)
  expect_error(bs_import_legacy(j), "rows measurement")
  x$candidate_spots[1] <- "bad"; write.csv(x, p, row.names = FALSE)
  expect_error(bs_import_legacy(p), "Invalid numeric")
})

test_that("validation rejects invalid arithmetic without integer overflow", {
  x <- result_fixture()
  y <- x; y$physical_slide_id[2] <- y$physical_slide_id[1]
  expect_error(bs_validate(y), "unique")
  y <- x; y$candidate_spots[1] <- .5
  expect_error(bs_validate(y), "whole counts")
  y <- x; y$candidate_spots[1] <- 2^53 + 2
  expect_error(bs_validate(y), "2\\^53")
  y <- x; y$red_area_mm2[1] <- Inf
  expect_error(bs_validate(y), "finite")
  y$red_area_mm2[1] <- NaN; expect_error(bs_validate(y), "finite")
  y$red_area_mm2[1] <- -1; expect_error(bs_validate(y), "finite")
  y <- x; y$red_area_mm2[1] <- 2
  expect_error(bs_validate(y), "exceeds")
  y <- x; y$operational_events[1] <- 9
  expect_error(bs_validate(y), "equal")
  y <- x; y$candidate_spots[3] <- 1; y$operational_events[3] <- 1
  expect_error(bs_validate(y), "positive tissue")
  expect_error(bs_validate(x, "biology"), "Only structure")
  expect_error(bs_validate(list()), "Expected")
  expect_error(bs_validate(x[0, ]), "columns")
  y <- x; y$candidate_spots[1:2] <- 2^53; y$unresolved_bulk_events <- 0
  y$operational_events <- y$candidate_spots
  expect_true(bs_validate(y))
  expect_error(bs_summarize(y), "Pooled counts")
})

test_that("QC revisions join by identity and cannot alter measurements", {
  x <- result_fixture()
  q <- data.frame(physical_slide_id = rev(x$physical_slide_id), decision = c("bad", "ok", "ok"))
  expect_equal(bs_summarize(x, "decision", q)$n_slides, c(2L, 1L))
  expect_false("decision" %in% names(x))
  q$tissue_area_mm2 <- 1
  expect_error(bs_summarize(x, "decision", q), "cannot replace")
  expect_error(bs_summarize(x, qc_revision = q[1, ]), "uniquely cover")
  expect_error(bs_summarize(x, c("organ", "organ")), "distinct")
  expect_error(bs_summarize(x, "absent"), "distinct")
  x$list <- I(list(1, 2, 3)); expect_error(bs_summarize(x, "list"), "atomic")
})

test_that("run comparison refuses guessed support and retains unknowns", {
  a <- result_fixture(); b <- a
  b$candidate_spots[2] <- 5; b$operational_events[2] <- 5
  out <- bs_compare(list(base = a, refined = b))
  expect_equal(out$candidate_spots_delta, c(0, 2, 0))
  b$support_id[2] <- "different"
  expect_error(bs_compare(list(base = a, refined = b)), "support")
  out <- bs_compare(list(base = a, refined = b), "union")
  expect_true(is.na(out$candidate_spots_delta[2]))
  b <- a[-1, ]; out <- bs_compare(list(base = a, refined = b), "union")
  expect_true(is.na(out$candidate_spots_second[1]))
  expect_false(out$support_verified[1])
  expect_equal(attr(bs_compare(list(base = a, refined = b)), "excluded_ids")$first, "001")
  expect_error(bs_compare(list(a, b)), "named list")
  b$physical_slide_id <- c("other", "other2")
  expect_error(bs_compare(list(base = a, refined = b)), "No slides")
  a$support_id <- NULL; b <- a
  expect_error(bs_compare(list(base = a, refined = b)), "support")
  a$regions <- b$regions <- c("r1", "r2", "r3")
  a$acquired_pixels <- b$acquired_pixels <- 100
  expect_true(all(bs_compare(list(base = a, refined = b))$support_verified))
})

test_that("numeric boundaries and duplicate headers fail before information loss", {
  x <- result_fixture()
  x$candidate_spots[1] <- 2^53; x$unresolved_bulk_events[1] <- 1
  x$operational_events[1] <- 2^53
  expect_error(bs_validate(x), "Combined event counts")
  x <- result_fixture(); x$candidate_spots[1:2] <- c(2^53, 1)
  x$unresolved_bulk_events <- 0; x$operational_events <- x$candidate_spots
  expect_error(bs_summarize(x), "Pooled counts")
  p <- tempfile(fileext = ".csv"); on.exit(unlink(p))
  writeLines(c("physical_slide_id,physical_slide_id", "a,b"), p)
  expect_error(bs_import_legacy(p), "Duplicate column")
})

test_that("extreme finite inputs never produce infinite output metrics", {
  x <- result_fixture()
  x$tissue_area_mm2[1:2] <- 1e308
  expect_error(bs_summarize(x), "Pooled measurements")
  huge <- result_fixture()[1, ]; huge$tissue_area_mm2 <- 1e308; huge$red_area_mm2 <- 1e308
  expect_equal(bs_summarize(huge)$red_percent, 100)
  x <- result_fixture(); x$tissue_area_mm2[1] <- 1e-320; x$red_area_mm2[1] <- 0
  expect_error(bs_compare(list(a = x, b = x)), "Derived rates")
})
