rscript <- function() file.path(R.home("bin"), "Rscript")

# Start an R process that starts `children` sleeping R processes through
# processx, each of which starts `grandchildren` more. Every started PID is
# recorded. Scripts are files, so no command line needs shell quoting.
process_tree <- function(children = 1L, grandchildren = 0L) {
  dir <- tempfile("bloodspottr-tree-")
  dir.create(dir)
  record <- file.path(dir, "pids")
  spawn <- function(n, script) {
    c(sprintf("k <- lapply(seq_len(%d), function(i)", n),
      sprintf("  processx::process$new(%s, c(%s, %s)))",
              deparse(rscript()), deparse(script), deparse(record)),
      "cat(vapply(k, function(x) x$get_pid(), 1L), sep = '\\n',",
      "    file = commandArgs(TRUE)[1], append = TRUE)",
      "Sys.sleep(30)")
  }
  leaf <- file.path(dir, "leaf.R")
  writeLines("Sys.sleep(30)", leaf)
  middle <- file.path(dir, "middle.R")
  writeLines(spawn(grandchildren, leaf), middle)
  top <- file.path(dir, "top.R")
  writeLines(spawn(children, if (grandchildren > 0L) middle else leaf), top)
  root <- processx::process$new(rscript(), c(top, record))
  expected <- children * (1L + grandchildren)
  deadline <- Sys.time() + 30
  while (Sys.time() < deadline &&
         (!file.exists(record) || length(readLines(record)) < expected))
    Sys.sleep(0.05)
  pids <- if (file.exists(record)) as.integer(readLines(record)) else integer()
  list(root = root, pids = pids, dir = dir)
}

drop_tree <- function(tree) {
  tree$root$kill_tree()
  unlink(tree$dir, recursive = TRUE)
}

test_that("descendants are found at every depth, parents before their children", {
  skip_if_not_installed("processx"); skip_if_not_installed("ps")
  skip_if_not(ps::ps_is_supported()); skip_on_cran()
  tree <- process_tree(children = 1L, grandchildren = 1L)
  on.exit(drop_tree(tree), add = TRUE)
  expect_length(tree$pids, 2L)
  parents <- vapply(tree$pids, function(pid) ps::ps_ppid(ps::ps_handle(pid)), 1L)
  child <- tree$pids[parents == tree$root$get_pid()]
  grandchild <- tree$pids[parents == child]
  found <- vapply(.bs_process_children(tree$root$as_ps_handle()), ps::ps_pid, 1L)
  expect_identical(found, c(child, grandchild))
})

test_that("only processes started no earlier than the worker are asked for their parent", {
  skip_if_not_installed("processx"); skip_if_not_installed("ps")
  skip_if_not(ps::ps_is_supported())
  # ps::ps_children() looks up the parent of every process on the machine,
  # which on Windows costs a full process snapshot per process.
  older <- processx::process$new(rscript(), c("-e", "Sys.sleep(30)"))
  on.exit(older$kill(), add = TRUE)
  Sys.sleep(0.2)
  tree <- process_tree(children = 1L)
  on.exit(drop_tree(tree), add = TRUE)
  root <- tree$root$as_ps_handle()
  asked <- list()
  ppid <- ps::ps_ppid
  local_mocked_bindings(ps_ppid = function(p = ps::ps_handle()) {
    asked[[length(asked) + 1L]] <<- list(pid = ps::ps_pid(p),
                                         time = ps::ps_create_time(p))
    ppid(p)
  }, .package = "ps")
  found <- vapply(.bs_process_children(root), ps::ps_pid, 1L)
  expect_identical(found, tree$pids)
  asked_pids <- vapply(asked, `[[`, 1L, "pid")
  asked_times <- vapply(asked, function(x) as.numeric(x$time), 1)
  expect_false(Sys.getpid() %in% asked_pids)
  expect_false(older$get_pid() %in% asked_pids)
  started <- as.numeric(ps::ps_create_time(root))
  expect_true(all(asked_times >= started - .bs_time_tolerance))
  expect_lt(length(asked), length(ps::ps_pids()))
})

test_that("a child that vanishes during enumeration is skipped without emptying the result", {
  skip_if_not_installed("processx"); skip_if_not_installed("ps")
  skip_if_not(ps::ps_is_supported()); skip_on_cran()
  tree <- process_tree(children = 2L)
  on.exit(drop_tree(tree), add = TRUE)
  expect_length(tree$pids, 2L)
  vanished <- tree$pids[2L]
  handle <- ps::ps_handle
  local_mocked_bindings(ps_handle = function(pid = NULL, time = NULL) {
    if (identical(as.integer(pid), vanished))
      base::stop(structure(list(message = "synthetic ENOENT", errno = 2L),
        class = c("os_error", "ps_error", "error", "condition")))
    handle(pid, time)
  }, .package = "ps")
  found <- vapply(.bs_process_children(tree$root$as_ps_handle()), ps::ps_pid, 1L)
  expect_identical(found, tree$pids[1L])
})

test_that("a failure to list processes remains a failure", {
  skip_if_not_installed("ps")
  parent <- ps::ps_handle()
  local_mocked_bindings(ps_pids = function() {
    base::stop(structure(
      list(message = "synthetic process failure", errno = 13L),
      class = c("os_error", "ps_error", "error", "condition")))
  }, .package = "ps")
  expect_error(.bs_process_children(parent), "synthetic process failure",
               class = "os_error")
  expect_true(ps::ps_is_running(parent))
})

test_that("a parent that has exited is reported instead of matched by PID", {
  skip_if_not_installed("processx"); skip_if_not_installed("ps")
  skip_if_not(ps::ps_is_supported())
  p <- processx::process$new(rscript(), c("-e", "invisible(0)"))
  handle <- p$as_ps_handle()
  p$wait(10000)
  rm(p); invisible(gc())
  expect_error(.bs_process_children(handle), class = "no_such_process")
})
