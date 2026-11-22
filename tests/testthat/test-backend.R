worker_fixture <- function(code) {
  root <- tempfile(); dir.create(root)
  script <- file.path(root,"worker.R")
  writeLines(c('a <- commandArgs(TRUE); out <- a[2]; request <- jsonlite::read_json(a[1])', code), script)
  input <- file.path(root,"input.bin"); writeLines("original",input)
  list(root=root, input=input, backend=bs_backend(file.path(R.home("bin"),"Rscript"), script,version="test"))
}
worker_complete <- c('writeLines("model",file.path(out,"model.txt"))',
 'jsonlite::write_json(list(schema_version=1L,status="complete",artifacts=c("model.txt")),file.path(out,"result.json"),auto_unbox=FALSE)')
# Scalar status and version are protocol scalars; artifacts remains an array.
worker_complete[2] <- 'jsonlite::write_json(list(schema_version=1L,status="complete",artifacts=list("model.txt")),file.path(out,"result.json"),auto_unbox=TRUE)'

test_that("workers commit verified output and preserve input hashes", {
  skip_if_not_installed("processx")
  f <- worker_fixture(worker_complete); on.exit(unlink(f$root,recursive=TRUE))
  out <- file.path(f$root,"job")
  job <- bs_train(c(image=f$input),f$backend,out)
  expect_s3_class(job,"bs_job")
  expect_equal(job$status,"complete")
  expect_equal(unname(unlist(job$input_sha256)),digest::digest(file=f$input,algo="sha256"))
  expect_equal(unname(unlist(job$artifacts)),digest::digest(file=file.path(out,"model.txt"),algo="sha256"))
  expect_true(file.exists(file.path(out,"job-receipt.json")))
  expect_error(bs_train(c(image=f$input),f$backend,out),"new path")
})

test_that("failed workers never publish output", {
  skip_if_not_installed("processx")
  cases <- list(c('quit(status=2)'), c('Sys.sleep(5)'),
    c('writeLines("mutated",request$inputs$image)',worker_complete),
    c('jsonlite::write_json(list(schema_version=1,status="complete",artifacts=list("../input.bin")),file.path(out,"result.json"),auto_unbox=TRUE)'),
    c('writeLines("receipt",file.path(out,"job-receipt.json"))','jsonlite::write_json(list(schema_version=1,status="complete",artifacts=list("job-receipt.json")),file.path(out,"result.json"),auto_unbox=TRUE)'))
  for (code in cases) {
    f <- worker_fixture(code)
    out <- file.path(f$root,"job")
    expect_error(bs_analyze(c(image=f$input),f$backend,out,timeout=0.5))
    expect_false(file.exists(out))
    expect_length(list.files(f$root,pattern="bloodspottr-worker",all.files=TRUE),0)
    unlink(f$root,recursive=TRUE)
  }
})

test_that("worker paths cannot publish symlinks or aliased artifacts", {
  skip_if_not_installed("processx"); skip_on_os("windows")
  cases <- list(c('file.symlink(request$inputs$image,file.path(out,"escape"))',worker_complete),
    c(worker_complete[1], 'jsonlite::write_json(list(schema_version=1,status="complete",artifacts=list("model.txt","./model.txt")),file.path(out,"result.json"),auto_unbox=TRUE)'))
  for (code in cases) {
    f <- worker_fixture(code)
    expect_error(bs_train(c(image=f$input),f$backend,file.path(f$root,"job")), "links|alias")
    expect_false(file.exists(file.path(f$root,"job")))
    unlink(f$root,recursive=TRUE)
  }
})

test_that("backend input validation rejects missing and malformed settings", {
  f <- worker_fixture(worker_complete); on.exit(unlink(f$root,recursive=TRUE))
  out <- file.path(f$root,"job")
  expect_error(bs_backend("/nonexistent",version="1"),"not found")
  expect_error(bs_backend(f$backend$command,version=NA_character_),"version")
  expect_error(bs_backend(f$backend$command,version="1",operations=c("train","train")),"unique")
  expect_error(bs_run_backend(f$backend,"unknown",c(image=f$input),out=out),"Unsupported")
  expect_error(bs_train(setNames(f$input,NA_character_),f$backend,out),"named existing")
  expect_error(bs_train(c(image=f$input),f$backend,out,parameters=setNames(list(1),NA_character_)),"named list")
  expect_error(bs_train(c(image=f$input),f$backend,out,timeout=NA_real_),"finite")
  expect_error(bs_train(c(image=f$root),f$backend,out),"named existing")
})

test_that("timeout cleans owned child processes without killing unrelated jobs", {
  skip_if_not_installed("processx"); skip_on_os("windows")
  sleeper <- Sys.which("sleep"); skip_if(!nzchar(sleeper))
  unrelated <- processx::process$new(sleeper,"30",stdout="|",stderr="|")
  on.exit(unrelated$kill(),add=TRUE)
  f <- worker_fixture(c('child <- processx::process$new(Sys.which("sleep"),"30",cleanup=FALSE,cleanup_tree=FALSE)',
    'writeLines(as.character(child$get_pid()), request$parameters$pid_file)', 'Sys.sleep(30)'))
  on.exit(unlink(f$root,recursive=TRUE),add=TRUE)
  pid_file <- file.path(f$root,"child.pid")
  expect_error(bs_train(c(image=f$input),f$backend,file.path(f$root,"job"),parameters=list(pid_file=pid_file),timeout=2),"timed out")
  expect_true(file.exists(pid_file))
  pid <- as.integer(readLines(pid_file))
  running <- tryCatch(ps::ps_is_running(ps::ps_handle(pid)) && ps::ps_status(ps::ps_handle(pid)) != "zombie",error=function(e) FALSE)
  expect_false(running)
  expect_true(unrelated$is_alive())
})

test_that("backend specifications reject malformed commands, arguments and handles", {
  rscript <- file.path(R.home("bin"), "Rscript")
  expect_error(bs_backend(c(rscript, rscript), version = "1"), "one executable")
  expect_error(bs_backend("", version = "1"), "one executable")
  expect_error(bs_backend(rscript, args = 1, version = "1"), "args must be character")
  expect_error(bs_backend(rscript, args = NA_character_, version = "1"), "args must be character")
  expect_error(bs_backend(R.home("bin"), version = "1"), "not found")
  expect_error(bs_backend(rscript, version = "1", operations = "Train"), "unique operation names")
  input <- tempfile(); writeLines("original", input); on.exit(unlink(input))
  out <- tempfile()
  expect_error(bs_run_backend(list(command = rscript), "train", c(image = input), out = out),
               "Expected a bs_backend")
  forged <- structure(list(command = rscript, args = character(), version = "",
                           operations = "train"), class = "bs_backend")
  expect_error(bs_run_backend(forged, "train", c(image = input), out = out), "version")
  expect_false(file.exists(out))
})

test_that("worker parameters must be JSON-compatible before any process starts", {
  f <- worker_fixture('writeLines("started", file.path(dirname(a[2]), "started"))')
  on.exit(unlink(f$root, recursive = TRUE))
  out <- file.path(f$root, "job")
  rejected <- list(list(a = Inf), list(a = NA_real_), list(a = NA_character_),
                   list(a = factor("x")), list(a = 1i), list(a = sum),
                   list(a = list(b = 1, b = 2)), list(a = list(1, list(NaN))))
  for (parameters in rejected) {
    expect_error(bs_train(c(image = f$input), f$backend, out, parameters = parameters),
                 "finite JSON-compatible")
  }
  expect_error(bs_train(c(image = f$input), f$backend, out, parameters = "a"), "named list")
  expect_error(bs_train(c(image = f$input), f$backend, out, parameters = list(1)), "named list")
  expect_error(bs_train(c(image = f$input), f$backend, out, timeout = 0), "positive")
  expect_false(file.exists(out))
  expect_false(file.exists(file.path(f$root, "started")))
  expect_length(list.files(f$root, pattern = "bloodspottr-worker", all.files = TRUE), 0)
})

test_that("nested and explicitly null parameters reach the worker unchanged", {
  skip_if_not_installed("processx")
  f <- worker_fixture(c(
    'writeLines(as.character(request$parameters$nested$depth), file.path(out, "model.txt"))',
    worker_complete[2]))
  on.exit(unlink(f$root, recursive = TRUE))
  out <- file.path(f$root, "job")
  parameters <- list(threshold = NULL, nested = list(depth = 3L, labels = c("a", "b")),
                     flags = list(TRUE, "x", 0.5))
  job <- bs_analyze(c(image = f$input), f$backend, out, parameters = parameters)
  expect_identical(job$operation, "analyze")
  expect_identical(job$request$parameters, parameters)
  expect_identical(readLines(file.path(out, "model.txt")), "3")
  expect_identical(job$output, normalizePath(out, winslash = "/"))
})

test_that("incomplete worker contracts are rejected without publishing output", {
  skip_if_not_installed("processx")
  result <- function(fields) paste0("jsonlite::write_json(list(", fields,
    '),file.path(out,"result.json"),auto_unbox=TRUE)')
  cases <- list(
    list(code = "invisible(NULL)", error = "did not produce result.json"),
    list(code = 'dir.create(file.path(out,"result.json"))',
         error = "did not produce result.json"),
    list(code = c(worker_complete[1],
                  result('schema_version=1,status="failed",artifacts=list("model.txt")')),
         error = "completion contract"),
    list(code = c(worker_complete[1],
                  result('schema_version=2,status="complete",artifacts=list("model.txt")')),
         error = "completion contract"),
    list(code = result('schema_version=1,status="complete",artifacts=list()'),
         error = "nonempty array"),
    list(code = c(worker_complete[1],
                  result('schema_version=1,status="complete",artifacts=list(model="model.txt")')),
         error = "nonempty array"),
    list(code = result('schema_version=1,status="complete",artifacts=list("absent.txt")'),
         error = "artifacts are missing"),
    list(code = c('dir.create(file.path(out,"folder"))',
                  result('schema_version=1,status="complete",artifacts=list("folder")')),
         error = "artifacts are missing"),
    list(code = c(worker_complete[1],
                  result('schema_version=1,status="complete",artifacts=list("model.txt","model.txt")')),
         error = "paths are invalid"),
    list(code = c('writeLines("{}",a[1])', worker_complete),
         error = "changed the request manifest"),
    list(code = c("unlink(a[1])", worker_complete),
         error = "changed the request manifest"))
  for (case in cases) {
    f <- worker_fixture(case$code)
    out <- file.path(f$root, "job")
    expect_error(bs_train(c(image = f$input), f$backend, out), case$error)
    expect_false(file.exists(out))
    expect_length(list.files(f$root, pattern = "bloodspottr-worker", all.files = TRUE), 0)
    expect_identical(readLines(f$input), "original")
    unlink(f$root, recursive = TRUE)
  }
})

test_that("worker failures report standard error and oversized logs are refused", {
  skip_if_not_installed("processx")
  f <- worker_fixture(c('message("model weights are unreadable")', "quit(status = 4)"))
  on.exit(unlink(f$root, recursive = TRUE), add = TRUE)
  expect_error(bs_train(c(image = f$input), f$backend, file.path(f$root, "job")),
               "model weights are unreadable")
  noisy <- worker_fixture(c('chunk <- strrep("x", 1024^2)',
                            "for (i in 1:11) cat(chunk)", worker_complete))
  on.exit(unlink(noisy$root, recursive = TRUE), add = TRUE)
  out <- file.path(noisy$root, "job")
  expect_error(bs_train(c(image = noisy$input), noisy$backend, out), "log exceeds 10 MiB")
  expect_false(file.exists(out))
})
