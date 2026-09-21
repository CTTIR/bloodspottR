export_fixture <- function() {
  x <- data.frame(physical_slide_id = c("001", "002"), organ = c("liver", "spleen"),
    tissue_area_mm2 = c(1, 2), red_area_mm2 = c(.1, NA_real_),
    candidate_spots = c(1, NA_real_), unresolved_bulk_events = c(0, NA_real_),
    operational_events = c(1, NA_real_), note = c("=SUM(1,2)", "<script>alert(1)</script>"))
  x$nested <- I(list(list(reader = "RH", classes = c("ery", "nucleus")), list()))
  structure(list(schema_version = "1.0", slides = x, groups = data.frame(),
                 provenance = list(scope = "<unvalidated>")), class = "bs_result")
}

test_that("export preserves canonical data and escapes presentation strings", {
  x <- export_fixture(); p <- tempfile(); on.exit(unlink(p, recursive = TRUE))
  expect_equal(bs_export_results(x, p), normalizePath(p))
  expect_true(all(c("Slides.csv", "Groups.csv", "Methods.csv", "results.json", "SHA256SUMS") %in% list.files(p)))
  z <- bs_import_legacy(p)
  expect_equal(z$slides$note, x$slides$note)
  expect_equal(z$slides$physical_slide_id, x$slides$physical_slide_id)
  expect_equal(z$slides$candidate_spots, x$slides$candidate_spots)
  expect_true(is.na(z$slides$red_area_mm2[2]))
  csv <- read.csv(file.path(p, "Slides.csv"), colClasses = "character", na.strings = "")
  expect_equal(csv$note[1], "'=SUM(1,2)")
  expect_match(csv$nested[1], '"reader":"RH"', fixed = TRUE)
  manifest <- readLines(file.path(p, "SHA256SUMS"))
  for (line in manifest) {
    parts <- strsplit(line, "  ", fixed = TRUE)[[1]]
    expect_identical(parts[1], digest::digest(file = file.path(p, parts[2]), algo = "sha256"))
  }
  expect_error(bs_export_results(x, p), "already exists")
  expect_error(bs_export_results(x, tempfile(), NA), "TRUE or FALSE")
  expect_error(bs_export_results(x$slides, tempfile()), "bs_result")
  expect_error(bs_export_results(x, NA_character_), "nonempty")
  expect_error(bs_export_results(x, file.path(tempfile(), "missing")), "Parent directory")
})

test_that("HTML report escapes untrusted text and exposes unavailable data", {
  x <- export_fixture(); p <- tempfile(); on.exit(unlink(p, recursive = TRUE))
  path <- bs_report(x, p, title = '<img src=x onerror="x">', author = "A&B", background = "<script>")
  html <- paste(readLines(path), collapse = "\n")
  expect_match(html, "&lt;img src=x onerror=&quot;x&quot;&gt;", fixed = TRUE)
  expect_match(html, "A&amp;B", fixed = TRUE)
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_match(html, "Not available", fixed = TRUE)
  expect_match(html, "No rows in this table.", fixed = TRUE)
  expect_match(html, "Exploratory", fixed = TRUE)
  expect_true(any(grepl("  report.html$", readLines(file.path(p, "SHA256SUMS")))))
  expect_error(bs_report(x, tempfile(), title = NA), "scalar")
  expect_error(bs_report(x, file.path(tempfile(), "missing")), "Parent directory")
})

test_that("Excel strings are literal and nested groups serialize deliberately", {
  skip_if_not_installed("openxlsx2")
  x <- export_fixture()
  x$groups <- bs_summarize(x, "organ")
  x$slides$factor <- factor(c("@formula", "plain"))
  x$slides$qc <- data.frame(flag = c(TRUE, FALSE), reviewer = c("RH", "unknown"))
  p <- tempfile(); on.exit(unlink(p, recursive = TRUE))
  bs_export_results(x, p, xlsx = TRUE)
  expect_true(file.exists(file.path(p, "results.xlsx")))
  wb <- openxlsx2::read_xlsx(file.path(p, "results.xlsx"), sheet = "Slides")
  expect_equal(wb$note, x$slides$note)
  expect_equal(wb$physical_slide_id, x$slides$physical_slide_id)
  expect_equal(wb$candidate_spots[1], 1)
  expect_true(is.na(wb$candidate_spots[2]))
  unz <- tempfile(); dir.create(unz); on.exit(unlink(unz, recursive = TRUE), add = TRUE)
  utils::unzip(file.path(p, "results.xlsx"), exdir = unz)
  xml <- paste(readLines(file.path(unz, "xl/worksheets/sheet1.xml"), warn = FALSE), collapse = "")
  expect_false(grepl("<f[ >]", xml))
  expect_equal(jsonlite::fromJSON(read.csv(file.path(p, "Slides.csv"))$qc[1])$reviewer, "RH")
  z <- bs_import_legacy(file.path(p, "results.json"))
  expect_equal(nrow(z$groups), 2)
  expect_equal(z$slides$qc, x$slides$qc)
})

test_that("CSV formula guards cover whitespace and preserve numeric signs", {
  x <- export_fixture()
  x$slides$note <- c(" \t+1", "-2")
  x$slides$extra <- c(-2, 0)
  p <- tempfile(); on.exit(unlink(p, recursive = TRUE))
  bs_export_results(x, p)
  z <- read.csv(file.path(p, "Slides.csv"), colClasses = "character")
  expect_equal(z$note, paste0("'", x$slides$note))
  expect_equal(z$extra, c("-2", "0"))
  x$groups <- NULL
  q <- tempfile(); on.exit(unlink(q, recursive = TRUE), add = TRUE)
  expect_true(file.exists(bs_report(x, q)))
})

test_that("invalid result schema is rejected on reimport", {
  p <- tempfile(fileext = ".json"); on.exit(unlink(p))
  x <- unclass(export_fixture()); x$schema_version <- "99"
  jsonlite::write_json(x, p, dataframe = "rows", auto_unbox = TRUE)
  expect_error(bs_import_legacy(p), "Unsupported result schema")
})

test_that("failed serialization leaves no delivery or staging residue", {
  x <- export_fixture(); parent <- tempfile(); dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE))
  x$provenance <- globalenv()
  expect_error(bs_export_results(x, file.path(parent, "failed")))
  expect_length(list.files(parent, all.files = TRUE, no.. = TRUE), 0)
  expect_error(bs_report(x, file.path(parent, "failed-report")))
  expect_length(list.files(parent, all.files = TRUE, no.. = TRUE), 0)
})

test_that("entirely missing canonical measurement columns stay numeric missing", {
  x <- export_fixture()
  for (nm in c("red_area_mm2", "candidate_spots", "unresolved_bulk_events", "operational_events"))
    x$slides[[nm]] <- rep(NA_real_, 2)
  p <- tempfile(); on.exit(unlink(p, recursive = TRUE))
  bs_export_results(x, p)
  z <- bs_import_legacy(file.path(p, "results.json"))
  expect_type(z$slides$candidate_spots, "double")
  expect_true(all(is.na(z$slides$candidate_spots)))
})
