rscript <- function() file.path(R.home("bin"), "Rscript")

# Write `code` to an R script in `dir`. Scripts are files rather than
# `Rscript -e`: on Windows a killed `Rscript -e` leaves its temporary
# expression file behind.
r_script <- function(code, dir = tempfile("bloodspottr-script-")) {
  dir.create(dir, showWarnings = FALSE)
  path <- tempfile("script-", tmpdir = dir, fileext = ".R")
  writeLines(code, path)
  path
}

# Start an R process that starts `children` sleeping R processes through
# processx, each of which starts `grandchildren` more. Parents record the PIDs
# they start. Every R process also records itself once its script runs, and
# the tree is returned only after all of them have: on Linux the R front end
# is a shell script whose short-lived helpers are children of the starting
# process until R itself runs.
process_tree <- function(children = 1L, grandchildren = 0L) {
  dir <- tempfile("bloodspottr-tree-")
  dir.create(dir)
  record <- file.path(dir, "pids")
  ready <- file.path(dir, "ready")
  announce <- sprintf("cat(Sys.getpid(), '\\n', sep = '', file = %s, append = TRUE)",
                      deparse(ready))
  spawn <- function(n, script) {
    c(announce,
      sprintf("k <- lapply(seq_len(%d), function(i)", n),
      sprintf("  processx::process$new(%s, %s))", deparse(rscript()), deparse(script)),
      sprintf("cat(vapply(k, function(x) x$get_pid(), 1L), sep = '\\n', file = %s,",
              deparse(record)),
      "    append = TRUE)",
      "Sys.sleep(30)")
  }
  leaf <- file.path(dir, "leaf.R")
  writeLines(c(announce, "Sys.sleep(30)"), leaf)
  middle <- file.path(dir, "middle.R")
  writeLines(spawn(grandchildren, leaf), middle)
  top <- file.path(dir, "top.R")
  writeLines(spawn(children, if (grandchildren > 0L) middle else leaf), top)
  root <- processx::process$new(rscript(), top)
  expected <- children * (1L + grandchildren)
  lines <- function(path) if (file.exists(path)) readLines(path) else character()
  deadline <- Sys.time() + 30
  while (Sys.time() < deadline &&
         (length(lines(record)) < expected || length(lines(ready)) < expected + 1L))
    Sys.sleep(0.05)
  list(root = root, pids = as.integer(lines(record)), dir = dir)
}

drop_tree <- function(tree) {
  tree$root$kill_tree()
  unlink(tree$dir, recursive = TRUE)
}

# Check descendant handles found below `root` (a PID) against the `fixture`
# PIDs a test started: every fixture process is found, each found process
# names the root or an earlier found process as its parent (so the list is
# real descendants, parents first), and anything else found is a console host,
# which Windows starts as a child of every console program.
expect_descendants <- function(found, root, fixture) {
  pids <- vapply(found, ps::ps_pid, 1L)
  parents <- vapply(found, function(h)
    tryCatch(ps::ps_ppid(h), error = function(e) NA_integer_), 1L)
  expect_true(all(fixture %in% pids))
  expect_false(anyNA(parents))
  for (i in seq_along(pids))
    expect_true(parents[[i]] %in% c(root, pids[seq_len(i - 1L)]))
  extra <- found[!pids %in% fixture]
  names <- vapply(extra, function(h)
    tryCatch(tolower(ps::ps_name(h)), error = function(e) NA_character_), "")
  allowed <- if (isTRUE(ps::ps_os_type()[["WINDOWS"]])) "conhost.exe" else character()
  expect_true(all(names %in% allowed), info = paste(names, collapse = ", "))
  invisible(pids)
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
  expect_length(child, 1L)
  expect_length(grandchild, 1L)
  pids <- expect_descendants(.bs_process_children(tree$root$as_ps_handle()),
                             tree$root$get_pid(), tree$pids)
  expect_lt(match(child, pids), match(grandchild, pids))
})

test_that("only processes started no earlier than the worker are asked for their parent", {
  skip_if_not_installed("processx"); skip_if_not_installed("ps")
  skip_if_not(ps::ps_is_supported())
  # ps::ps_children() looks up the parent of every process on the machine,
  # which on Windows costs a full process snapshot per process.
  sleeper <- r_script("Sys.sleep(30)")
  on.exit(unlink(dirname(sleeper), recursive = TRUE), add = TRUE)
  older <- processx::process$new(rscript(), sleeper)
  on.exit(older$kill_tree(), add = TRUE)
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
  found <- .bs_process_children(root)
  # Taken before the checks below, which look up parents themselves.
  asked_pids <- vapply(asked, `[[`, 1L, "pid")
  asked_times <- vapply(asked, function(x) as.numeric(x$time), 1)
  expect_descendants(found, tree$root$get_pid(), tree$pids)
  expect_false(Sys.getpid() %in% asked_pids)
  expect_false(older$get_pid() %in% asked_pids)
  started <- as.numeric(ps::ps_create_time(root))
  expect_true(all(asked_times >= started - .bs_time_tolerance))
  expect_lt(length(asked_pids), length(ps::ps_pids()))
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
  found <- .bs_process_children(tree$root$as_ps_handle())
  pids <- expect_descendants(found, tree$root$get_pid(), tree$pids[1L])
  expect_false(vanished %in% pids)
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
  script <- r_script("invisible(0)")
  on.exit(unlink(dirname(script), recursive = TRUE), add = TRUE)
  p <- processx::process$new(rscript(), script)
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
    # A reaped zombie can vanish between the two calls.
    running <- tryCatch(ps::ps_is_running(handle) && ps::ps_status(handle) != "zombie",
                        no_such_process = function(e) FALSE)
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
  found <- expect_descendants(.bs_linux_descendants(root), tree$root$get_pid(),
                              tree$pids)
  expect_setequal(found[1:2], children)
  expect_identical(listed, 0L)
  # With every fixture process started, both methods see the same tree.
  scanned <- expect_descendants(.bs_process_children(root), tree$root$get_pid(),
                                tree$pids)
  expect_setequal(scanned, found)
})

test_that("Linux task lists of a finished worker are empty", {
  skip_if_not_installed("processx"); skip_if_not_installed("ps")
  skip_if_not(ps::ps_is_supported())
  skip_if(.bs_watch_mode() != "children", "no /proc task children lists here")
  script <- r_script("invisible(0)")
  on.exit(unlink(dirname(script), recursive = TRUE), add = TRUE)
  p <- processx::process$new(rscript(), script)
  handle <- p$as_ps_handle()
  p$wait(10000)
  expect_identical(.bs_linux_descendants(handle), list())
})
