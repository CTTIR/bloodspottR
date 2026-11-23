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

# Run `code` (lines of R) as a worker through .bs_run_process() while counting
# process listings (ps_pids) and parent lookups (ps_ppid). A listing counts as
# "during" the run if the worker has not yet written `flag`, its last action.
count_listings <- function(code, timeout = 10) {
  dir <- tempfile("bloodspottr-count-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  flag <- file.path(dir, "finished")
  script <- file.path(dir, "worker.R")
  writeLines(c("a <- commandArgs(TRUE)", code, "file.create(a[1])"), script)
  log <- list(during = logical(), ppid = 0L)
  pids <- ps::ps_pids
  ppid <- ps::ps_ppid
  local_mocked_bindings(
    ps_pids = function() {
      log$during <<- c(log$during, !file.exists(flag))
      pids()
    },
    ps_ppid = function(p = ps::ps_handle()) {
      log$ppid <<- log$ppid + 1L
      ppid(p)
    }, .package = "ps")
  result <- .bs_run_process(rscript(), c(script, flag), timeout)
  c(list(result = result), log)
}

test_that("the poll loop lists no processes while the worker runs", {
  skip_if_not_installed("processx"); skip_if_not_installed("ps")
  skip_if_not(ps::ps_is_supported())
  skip_if(.bs_watch_mode() == "scan", "descendants are observed by listing processes here")
  run <- count_listings("Sys.sleep(1)")
  expect_identical(run$result$status, 0L)
  expect_false(any(run$during))
  # The one listing is the environment-marker sweep after the worker exited.
  expect_identical(length(run$during), 1L)
})

test_that("without observation a finished worker costs one listing and no parent lookups", {
  skip_if_not_installed("processx"); skip_if_not_installed("ps")
  skip_if_not(ps::ps_is_supported())
  # The Windows mode: each parent lookup there is a snapshot of all processes.
  local_mocked_bindings(.bs_watch_mode = function() "none")
  run <- count_listings("Sys.sleep(0.5)")
  expect_identical(run$result$status, 0L)
  expect_false(any(run$during))
  expect_identical(length(run$during), 1L)
  expect_identical(run$ppid, 0L)
})

test_that("without observation a helper left by the worker is still stopped", {
  skip_if_not_installed("processx"); skip_if_not_installed("ps")
  skip_if_not(ps::ps_is_supported()); skip_on_os(c("windows", "mac"))
  sleeper <- Sys.which("sleep"); skip_if(!nzchar(sleeper))
  local_mocked_bindings(.bs_watch_mode = function() "none")
  for (timeout in c(10, 2)) {
    dir <- tempfile("bloodspottr-helper-")
    dir.create(dir)
    identity_file <- file.path(dir, "child.rds")
    script <- file.path(dir, "worker.R")
    writeLines(c('a <- commandArgs(TRUE)',
      'child <- processx::process$new(Sys.which("sleep"), "30", cleanup = FALSE, cleanup_tree = FALSE)',
      'saveRDS(list(pid = child$get_pid(), time = ps::ps_create_time(child$as_ps_handle())), a[1])',
      # A finished worker leaves the helper behind; a slow one times out.
      if (timeout < 5) "Sys.sleep(30)" else "Sys.sleep(0.5)"), script)
    result <- .bs_run_process(rscript(), c(script, identity_file), timeout)
    identity <- readRDS(identity_file)
    handle <- ps::ps_handle(identity$pid, time = identity$time)
    running <- ps::ps_is_running(handle) && ps::ps_status(handle) != "zombie"
    if (running) ps::ps_kill(handle)
    expect_false(running)
    expect_identical(result$timeout, timeout < 5)
    unlink(dir, recursive = TRUE)
  }
})

test_that("Linux task lists find descendants at every depth, parents first", {
  skip_if_not_installed("processx"); skip_if_not_installed("ps")
  skip_if_not(ps::ps_is_supported()); skip_on_cran()
  skip_if(.bs_watch_mode() != "children", "no /proc task children lists here")
  tree <- process_tree(children = 2L, grandchildren = 1L)
  on.exit(drop_tree(tree), add = TRUE)
  expect_length(tree$pids, 4L)
  root <- tree$root$as_ps_handle()
  parents <- vapply(tree$pids, function(pid) ps::ps_ppid(ps::ps_handle(pid)), 1L)
  children <- tree$pids[parents == tree$root$get_pid()]
  listed <- 0L
  pids <- ps::ps_pids
  local_mocked_bindings(ps_pids = function() {
    listed <<- listed + 1L
    pids()
  }, .package = "ps")
  found <- vapply(.bs_linux_descendants(root), ps::ps_pid, 1L)
  expect_setequal(found, tree$pids)
  expect_setequal(found[1:2], children)
  expect_identical(listed, 0L)
  expect_setequal(vapply(.bs_process_children(root), ps::ps_pid, 1L), found)
})

test_that("Linux task lists of a finished worker are empty", {
  skip_if_not_installed("processx"); skip_if_not_installed("ps")
  skip_if_not(ps::ps_is_supported())
  skip_if(.bs_watch_mode() != "children", "no /proc task children lists here")
  p <- processx::process$new(rscript(), c("-e", "invisible(0)"))
  handle <- p$as_ps_handle()
  p$wait(10000)
  expect_identical(.bs_linux_descendants(handle), list())
})
