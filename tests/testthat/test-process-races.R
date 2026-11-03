test_that("a disappearing child triggers fresh enumeration instead of empty success", {
  skip_if_not_installed("ps")
  parent <- ps::ps_handle()
  observed <- list(parent)
  calls <- 0L
  missing <- structure(list(message = "synthetic ENOENT", errno = 2L),
    class = c("os_error", "ps_error", "error", "condition"))
  local_mocked_bindings(ps_children = function(p, recursive) {
    calls <<- calls + 1L
    expect_identical(p, parent)
    expect_true(recursive)
    if (calls == 1L) base::stop(missing)
    observed
  }, .package = "ps")
  expect_identical(.bs_process_children(parent), observed)
  expect_identical(calls, 2L)
})

test_that("repeated enumeration failure and unrelated OS errors remain failures", {
  skip_if_not_installed("ps")
  parent <- ps::ps_handle()
  calls <- 0L
  errno <- 2L
  local_mocked_bindings(ps_children = function(...) {
    calls <<- calls + 1L
    base::stop(structure(list(message = "synthetic process failure", errno = errno),
      class = c("os_error", "ps_error", "error", "condition")))
  }, .package = "ps")
  expect_error(.bs_process_children(parent), "synthetic process failure", class = "os_error")
  expect_identical(calls, 3L)
  expect_true(ps::ps_is_running(parent))
  calls <- 0L
  errno <- 13L
  expect_error(.bs_process_children(parent), "synthetic process failure", class = "os_error")
  expect_identical(calls, 1L)
})
