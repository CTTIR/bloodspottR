# Presentation helpers: the overview plot, report tables and the slide table.

test_that("the overview plot draws complete slides and restores the device state", {
  demo <- bs_viewer_example()
  unlink(demo$directory, recursive = TRUE)
  d <- .bs_app_results(demo$results)$slides
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  before <- graphics::par(c("mar", "mfrow", "xpd", "cex"))
  drawn <- .bs_app_scatter(d, "organ", "Tissue")
  expect_identical(graphics::par(c("mar", "mfrow", "xpd", "cex")), before)
  # The slide with a missing red area is omitted, never plotted as zero.
  expect_identical(nrow(drawn), sum(is.finite(d$red_area_mm2) & d$tissue_area_mm2 > 0))
  expect_false("DEMO-008" %in% drawn$slide_id)
  # Missing group labels and more groups than base colors still draw.
  many <- d[rep(1L, 30L), ]
  many$slide_id <- sprintf("S%02d", 1:30)
  many$organ <- c(NA, "", sprintf("tissue %02d", 3:30))
  expect_identical(nrow(.bs_app_scatter(many, "organ", "Tissue")), 30L)
  # All-zero measurements keep finite axis limits.
  zero <- d[1L, ]
  zero$red_area_mm2 <- 0
  zero$candidate_spots <- 0
  expect_identical(nrow(.bs_app_scatter(zero)), 1L)
  expect_error(.bs_app_scatter(d[d$slide_id == "DEMO-008", ]), "complete measurements",
               class = "bloodspottr_error")
})

test_that("overview colors are distinct for common group counts", {
  palette <- unname(grDevices::palette.colors(8L, "Okabe-Ito"))[c(6L, 7L, 4L, 2L, 8L, 3L, 1L)]
  expect_length(unique(palette), 7L)
  # Yellow has too little contrast on a white panel and is not used.
  expect_false("#F0E442" %in% toupper(palette))
  combos <- paste(rep(palette, length.out = 28L), rep(c(16, 17, 15, 18), length.out = 28L))
  expect_identical(anyDuplicated(combos), 0L)
})

test_that("report tables round rates for display but keep whole counts exact", {
  expect_identical(.bs_display_number(c(1.4444444444444444, 0.65, NA, 45, 2^53, 0)),
                   c("1.44444", "0.65", NA, "45", "9007199254740992", "0"))
  expect_identical(.bs_display_number(c("a", NA)), c("a", NA))
  expect_identical(.bs_display_number(c(TRUE, NA)), c("TRUE", NA))
  expect_identical(.bs_display_number(3L), "3")
  x <- bs_example()
  x$groups <- bs_summarize(x, strata = "organ")
  out <- tempfile("bs-report-")
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  html <- paste(readLines(bs_report(x, out)), collapse = "\n")
  expect_match(html, "<td>1.44444</td>", fixed = TRUE)
  expect_false(grepl("1.4444444", html, fixed = TRUE))
  expect_match(html, "six significant digits", fixed = TRUE)
  # Machine-readable outputs keep 15 significant digits.
  restored <- bs_import_legacy(file.path(out, "results.json"))
  expect_equal(restored$groups$red_percent[1], x$groups$red_percent[1], tolerance = 1e-14)
  csv <- utils::read.csv(file.path(out, "Groups.csv"))
  expect_equal(csv$red_percent[1], x$groups$red_percent[1], tolerance = 1e-14)
})

test_that("the slide table shows whole counts without decimals", {
  d <- .bs_app_results(bs_example())$slides
  shown <- .bs_app_table(d)
  expect_type(shown$candidate_spots, "integer")
  expect_type(shown$operational_events, "integer")
  expect_identical(as.numeric(shown$candidate_spots), d$candidate_spots)
  expect_type(shown$tissue_area_mm2, "double")
  big <- d
  big$candidate_spots[1] <- 2^40
  expect_type(.bs_app_table(big)$candidate_spots, "double")
})
