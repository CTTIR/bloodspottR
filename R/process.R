# Keep ancestry handles as well as an environment-marker sweep. Some platforms
# cannot inspect every descendant's environment. Handles retain creation
# times, so an unrelated process reusing a PID is never a kill target.
#
# The worker's liveness comes from its own processx handle. Listing the
# processes of the machine costs a handle per process, and on Windows each
# parent lookup a snapshot of all processes: polling that way made one short
# worker run take 15-20 s of system time on a busy Windows machine. So the poll
# loop lists processes only where descendants cannot be observed otherwise
# (see .bs_watch_mode()). At clean-up a worker that is still running (timeout,
# error, interrupt) is scanned once for descendants before it is stopped, and
# the environment marker is swept once in every case.
.bs_run_process <- function(command, args, timeout) {
  if (!requireNamespace("processx", quietly = TRUE) ||
      !requireNamespace("ps", quietly = TRUE) || !ps::ps_is_supported())
    .bs_abort("Worker execution requires processx and supported ps process inspection")
  logs <- tempfile("bloodspottr-process-")
  if (!dir.create(logs)) .bs_abort("Could not create worker log directory")
  on.exit(unlink(logs, recursive = TRUE), add = TRUE)
  stdout <- file.path(logs, "stdout")
  stderr <- file.path(logs, "stderr")
  max_log <- 10 * 1024^2
  # The worker and every descendant that keeps its environment inherit this
  # marker. No processx cleanup_tree: its finaliser would sweep a marker a
  # second time at garbage collection, after the sweep in cleanup() below.
  marker <- ps::ps_mark_tree()
  process <- tryCatch(
    processx::process$new(command, args, stdout = stdout, stderr = stderr),
    finally = Sys.unsetenv(marker))
  owned <- list(process$as_ps_handle())
  watch <- switch(.bs_watch_mode(), children = .bs_linux_descendants,
                  scan = .bs_process_children, none = NULL)
  adopt <- function(handles) {
    candidates <- c(owned, handles)
    keys <- vapply(candidates, function(p)
      paste(ps::ps_pid(p), ps::ps_create_time(p)), "")
    owned <<- candidates[!duplicated(keys)]
  }
  remember <- function(find) {
    adopt(tryCatch(find(owned[[1L]]), no_such_process = function(e) list()))
  }
  alive <- function(p) tryCatch(isTRUE(ps::ps_is_running(p)) &&
                                !identical(ps::ps_status(p), "zombie"),
                                no_such_process = function(e) FALSE)
  cleaned <- FALSE
  cleanup <- function() {
    if (cleaned) return(TRUE)
    # Observe before killing the worker, while ancestry can still be recovered.
    # Once the worker has exited, its children are no longer reachable through
    # it (ps_children() and .bs_process_children() refuse a gone parent).
    if (process$is_alive()) remember(.bs_process_children)
    # One environment-marker sweep; its handles are stopped and verified too.
    adopt(ps::ps_find_tree(marker))
    running <- Filter(alive, rev(owned))
    if (length(running)) ps::ps_kill(running)
    process$wait(1000)
    deadline <- proc.time()[["elapsed"]] + 1
    repeat {
      running <- vapply(owned, alive, logical(1))
      if (!any(running)) break
      if (proc.time()[["elapsed"]] >= deadline) return(FALSE)
      Sys.sleep(0.01)
    }
    cleaned <<- TRUE
    TRUE
  }
  on.exit({
    if (!cleaned) {
      result <- tryCatch(cleanup(), error = function(e) FALSE)
      if (!isTRUE(result)) cli::cli_warn("Worker cleanup could not be verified", call = NULL)
    }
  }, add = TRUE, after = FALSE)
  start <- proc.time()[["elapsed"]]
  timed_out <- FALSE
  # wait() returns as soon as the worker exits, so the growing interval only
  # spaces out log-size checks and observations, never the exit itself.
  interval <- .bs_poll_first
  while (process$is_alive()) {
    if (!is.null(watch)) remember(watch)
    if (any(file.info(c(stdout, stderr))$size > max_log))
      .bs_abort("Worker log exceeds 10 MiB; reduce worker verbosity")
    remaining <- timeout - (proc.time()[["elapsed"]] - start)
    if (remaining <= 0) { timed_out <- TRUE; break }
    process$wait(max(1, min(interval, remaining) * 1000))
    interval <- min(2 * interval, .bs_poll_last)
  }
  if (!cleanup()) .bs_abort("Worker cleanup could not be verified; output not committed")
  read_log <- function(path) {
    size <- file.info(path)$size
    if (size > max_log) .bs_abort("Worker log exceeds 10 MiB; reduce worker verbosity")
    if (size == 0) "" else readChar(path, size, useBytes = TRUE)
  }
  list(status = process$get_exit_status(), timeout = timed_out,
       stdout = read_log(stdout), stderr = read_log(stderr))
}

# Poll interval in seconds: first wait, and the cap it doubles up to.
.bs_poll_first <- 0.05
.bs_poll_last <- 0.5

# How descendants are observed while a worker runs:
# - "none" on Windows. Listing processes is expensive there, and the marker
#   sweep at clean-up reads the environment of the user's own processes.
# - "children" on Linux: the kernel lists each task's children in
#   /proc/<pid>/task/<tid>/children, so the worker's tree is followed without
#   listing other processes.
# - "scan" elsewhere (macOS, Linux without that file). macOS cannot read the
#   environment of protected system binaries such as /bin/sleep, so a helper
#   that outlives the worker is only found if it was seen while attached.
.bs_watch_mode <- function() {
  os <- ps::ps_os_type()
  if (isTRUE(os[["WINDOWS"]])) return("none")
  if (isTRUE(os[["LINUX"]]) && file.exists(file.path(
        "/proc", Sys.getpid(), "task", Sys.getpid(), "children")))
    return("children")
  "scan"
}

# Linux descendants of `parent` from /proc/<pid>/task/<tid>/children, without
# listing other processes. A child is kept only if it still names its parent
# and is not older than it, and a parent's list is used only if the parent is
# still the same process after it was read, so a reused PID is never adopted.
.bs_linux_descendants <- function(parent) {
  found <- list()
  frontier <- list(parent)
  seen <- ps::ps_pid(parent)
  while (length(frontier)) {
    nxt <- list()
    for (p in frontier) {
      pid <- ps::ps_pid(p)
      files <- Sys.glob(file.path("/proc", pid, "task", "*", "children"))
      kids <- unlist(lapply(files, function(f) tryCatch(
        scan(f, what = integer(), quiet = TRUE),
        error = function(e) integer())))
      # Use the list only if `p` is still the process it was read from.
      running <- tryCatch(ps::ps_is_running(p), error = function(e) FALSE)
      if (!isTRUE(running)) next
      for (kid in setdiff(unique(kids), seen)) {
        h <- tryCatch(ps::ps_handle(kid), error = function(e) NULL)
        if (is.null(h)) next
        ok <- tryCatch(identical(ps::ps_ppid(h), pid) &&
          as.numeric(ps::ps_create_time(h)) >=
            as.numeric(ps::ps_create_time(p)) - .bs_time_tolerance,
          error = function(e) FALSE)
        if (!ok) next
        seen <- c(seen, kid)
        found[[length(found) + 1L]] <- h
        nxt[[length(nxt) + 1L]] <- h
      }
    }
    frontier <- nxt
  }
  found
}

# Descendants of `parent`, found like ps::ps_children(recursive = TRUE) but
# without asking every process on the machine for its parent. ps_children()
# looks up the parent of every process, and on Windows each lookup takes a
# fresh snapshot of all processes, so one call grows with the square of the
# number of processes: seconds of system time per poll on a busy machine.
# A descendant never starts before its ancestors, so only processes started no
# earlier than `parent` need a parent lookup. Each candidate keeps the handle
# it was found with, so a process that exits during the walk is skipped on its
# own instead of failing or emptying the whole enumeration (ps 1.9.3 raised
# ENOENT when ps_children() created a second handle for a vanished child).
# A failure to list processes at all still propagates. Previously observed
# handles remain in `owned` above. Creation times are compared with the 10 ms
# tolerance that ps itself allows when it matches them, so a rounding
# difference between processx and ps cannot hide a child.
.bs_process_children <- function(parent) {
  root <- ps::ps_pid(parent)
  root_time <- as.numeric(ps::ps_create_time(parent))
  candidates <- lapply(setdiff(ps::ps_pids(), root), .bs_process_entry,
                       since = root_time - .bs_time_tolerance)
  candidates <- candidates[!vapply(candidates, is.null, logical(1))]
  pids <- vapply(candidates, `[[`, integer(1), "pid")
  ppids <- vapply(candidates, `[[`, integer(1), "ppid")
  times <- vapply(candidates, `[[`, numeric(1), "time")
  found <- integer()
  frontier <- root
  frontier_time <- root_time
  while (length(frontier)) {
    # A child is never older than its parent; this also rejects a process
    # whose recorded parent PID has since been reused by a descendant.
    parent_at <- match(ppids, frontier)
    hit <- which(!is.na(parent_at) &
                 times >= frontier_time[parent_at] - .bs_time_tolerance)
    # setdiff() guarantees termination even for impossible PID cycles.
    hit <- setdiff(hit, found)
    # Parents precede their children, so callers can stop leaves first.
    found <- c(found, hit)
    frontier <- pids[hit]
    frontier_time <- times[hit]
  }
  # As in ps_children(): fail with no_such_process if `parent` itself has gone,
  # because its PID may then belong to an unrelated process tree.
  ps::ps_ppid(parent)
  lapply(candidates[found], `[[`, "handle")
}

.bs_time_tolerance <- 0.01

# One process started no earlier than `since` (seconds since the epoch), with
# its parent PID, or NULL if it is older, has exited or cannot be inspected.
.bs_process_entry <- function(pid, since) {
  handle <- tryCatch(ps::ps_handle(pid), error = function(e) NULL)
  if (is.null(handle)) return(NULL)
  time <- as.numeric(ps::ps_create_time(handle))
  if (time < since) return(NULL)
  ppid <- tryCatch(ps::ps_ppid(handle), error = function(e) NULL)
  if (is.null(ppid) || is.na(ppid)) return(NULL)
  list(pid = as.integer(pid), ppid = as.integer(ppid), time = time,
       handle = handle)
}
