test_that("comparison direction and excluded physical slides survive delivery", {
  x <- bs_example()
  x$slides$support_id <- x$slides$physical_slide_id
  y <- x
  y$slides <- y$slides[-1, ]
  x$comparison <- bs_compare(list("baseline <A>" = x, followup = y))
  destination <- tempfile()
  on.exit(unlink(destination, recursive = TRUE))
  report <- bs_report(x, destination)
  restored <- bs_import_legacy(destination)
  expect_identical(attr(restored$comparison, "runs"), c("baseline <A>", "followup"))
  expect_identical(attr(restored$comparison, "excluded_ids"),
                   list(first = x$slides$physical_slide_id[1], second = character()))
  expect_equal(restored$comparison, x$comparison)
  html <- paste(readLines(report), collapse = "\n")
  expect_match(html, "First run: baseline &lt;A&gt;; second run: followup", fixed = TRUE)
  expect_match(html, x$slides$physical_slide_id[1], fixed = TRUE)
  raw <- jsonlite::read_json(file.path(destination, "results.json"))
  expect_equal(unlist(raw$comparison_context$runs), c("baseline <A>", "followup"))
  raw$comparison_context$runs <- list("same", "same")
  jsonlite::write_json(raw, file.path(destination, "results.json"), auto_unbox = TRUE)
  expect_error(bs_import_legacy(destination), "comparison context")
})

test_that("refresh keeps valid data if a required custom grouping disappears", {
  skip_if_not_installed("shiny")
  x <- bs_example(); x$slides$site <- x$slides$organ
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  save <- function(value) jsonlite::write_json(unclass(value), path,
    auto_unbox = TRUE, dataframe = "rows", na = "null")
  save(x)
  shiny::testServer(.bs_app_server(.bs_app_results(x), result_path = path, group_by = "site"), {
    session$setInputs(refresh_slide = 0, organ = "", qc = "", search = "")
    y <- x; y$slides$site <- NULL; save(y)
    session$setInputs(refresh_slide = 1)
    expect_identical(current()$slides$site, x$slides$site)
    expect_equal(nrow(filtered()), nrow(x$slides))
    expect_match(output$viewer_status, "Missing grouping column: site")
  })
})

test_that("annotation transforms cannot emit nonfinite coordinates", {
  p <- tempfile(fileext = ".csv"); on.exit(unlink(p))
  writeLines(c("x,y", "1e308,1"), p)
  row <- data.frame(annotations = p, scale_x = 2, scale_y = 1, offset_x = 0, offset_y = 0)
  expect_error(.bs_annotations(row), "Transformed annotation coordinates")
})

test_that("worker completion and failure clean observed descendants with inherited output", {
  skip_if_not_installed("processx"); skip_if_not_installed("ps")
  skip_on_os("windows")
  sleeper <- Sys.which("sleep"); skip_if(!nzchar(sleeper))
  unrelated <- processx::process$new(sleeper, "30")
  on.exit(unrelated$kill(), add = TRUE)
  for (status in c(0L, 7L)) {
    root <- tempfile(); dir.create(root)
    script <- file.path(root, "worker with spaces.R")
    identity_file <- file.path(root, "child.rds")
    writeLines(c('a <- commandArgs(TRUE)',
      'child <- processx::process$new(Sys.which("sleep"), "30", stdout="", stderr="", cleanup=FALSE, cleanup_tree=FALSE)',
      'saveRDS(list(pid=child$get_pid(), time=ps::ps_create_time(child$as_ps_handle())), a[1])',
      'cat(a[2]); cat("diagnostic", file=stderr()); Sys.sleep(0.5)',
      paste0('quit(status=', status, ')')), script)
    result <- .bs_run_process(file.path(R.home("bin"), "Rscript"),
      c(script, identity_file, "literal ; $value"), timeout = 3)
    identity <- readRDS(identity_file)
    # Retain the observed creation time: a reaped PID cannot be queried again,
    # and a subsequently reused PID must not become this test's cleanup target.
    handle <- ps::ps_handle(identity$pid, time = identity$time)
    running <- ps::ps_is_running(handle) && ps::ps_status(handle) != "zombie"
    if (running) ps::ps_kill(handle)
    expect_false(running)
    expect_identical(result$status, status)
    expect_false(result$timeout)
    expect_identical(result$stdout, "literal ; $value")
    expect_identical(result$stderr, "diagnostic")
    expect_true(unrelated$is_alive())
    unlink(root, recursive = TRUE)
  }
})

test_that("excessive worker output is rejected before receipt serialization", {
  skip_if_not_installed("processx"); skip_if_not_installed("ps")
  script <- tempfile(fileext = ".R"); on.exit(unlink(script))
  writeLines('cat(strrep("x", 11 * 1024^2)); Sys.sleep(5)', script)
  expect_error(.bs_run_process(file.path(R.home("bin"), "Rscript"), script, 3),
               "log exceeds 10 MiB", class = "bloodspottr_error")
})
