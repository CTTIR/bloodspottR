# Keep ancestry handles as well as processx's environment-marker cleanup.
# Some platforms cannot inspect every descendant's environment. Handles retain
# creation times, so an unrelated process reusing a PID is never a kill target.
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
  process <- processx::process$new(command, args, stdout = stdout, stderr = stderr,
                                  cleanup_tree = TRUE)
  owned <- list(process$as_ps_handle())
  remember <- function() {
    descendants <- tryCatch(.bs_process_children(owned[[1L]]),
      no_such_process = function(e) list())
    candidates <- c(owned, descendants)
    keys <- vapply(candidates, function(p)
      paste(ps::ps_pid(p), ps::ps_create_time(p)), "")
    owned <<- candidates[!duplicated(keys)]
  }
  alive <- function(p) tryCatch(isTRUE(ps::ps_is_running(p)) &&
                                !identical(ps::ps_status(p), "zombie"),
                                no_such_process = function(e) FALSE)
  cleaned <- FALSE
  cleanup <- function() {
    if (cleaned) return(TRUE)
    # Observe before killing the parent, while ancestry can still be recovered.
    remember()
    process$kill_tree()
    for (p in rev(owned)) if (alive(p)) ps::ps_kill(p)
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
  while (process$is_alive()) {
    remember()
    if (any(file.info(c(stdout, stderr))$size > max_log))
      .bs_abort("Worker log exceeds 10 MiB; reduce worker verbosity")
    remaining <- timeout - (proc.time()[["elapsed"]] - start)
    if (remaining <= 0) { timed_out <- TRUE; break }
    process$wait(min(100, max(1, remaining * 1000)))
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
