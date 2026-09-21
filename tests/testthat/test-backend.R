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
