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

# ps 1.9.3 can expose ENOENT when a child exits between the process snapshot
# and handle creation. Retry a complete enumeration, never substitute an empty
# result for that error. Previously observed handles remain in `owned` above.
.bs_process_children <- function(parent) {
  for (attempt in seq_len(3L)) {
    retry <- FALSE
    children <- tryCatch(ps::ps_children(parent, recursive = TRUE),
      os_error = function(e) {
        # Preserve the original ps condition class and errno when rethrowing.
        if (!identical(e$errno, 2L) || attempt == 3L) base::stop(e)
        retry <<- TRUE
        NULL
      })
    if (!retry) return(children)
  }
}
