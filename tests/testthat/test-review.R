review_fixture <- function(n = 2L) {
  root <- tempfile(); dir.create(root)
  image <- file.path(root, "image.bin"); writeBin(as.raw(1:10), image)
  images <- data.frame(item_id = paste0("i", seq_len(n)), parent_id = "p", path = image)
  plan <- bs_review_plan(images, out = file.path(root, "queue"))
  csv <- file.path(root, "points.csv"); writeLines("class,x,y", csv)
  list(root = root, images = images, plan = plan, csv = csv,
       assertions = list(ery = "absent", nuclei = "unreviewed", vessel = "unreviewed"))
}

test_that("saved empty reviews require assertions and preserve idempotence", {
  f <- review_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  expect_error(bs_review_confirm(f$plan, "i2", f$csv, f$assertions), "next incomplete")
  expect_error(bs_review_confirm(f$plan, "i1", f$csv, as.list(c(ery="unreviewed",nuclei="unreviewed",vessel="unreviewed"))), "assertions")
  r <- bs_review_confirm(f$plan, "i1", f$csv, f$assertions)
  expect_equal(r$n_points, 0L)
  r2 <- bs_review_confirm(f$plan, "i1", f$csv, f$assertions)
  expect_equal(r2$annotation_sha256, r$annotation_sha256)
  writeLines(c("class,x,y", "Cell_nucleus,4,5"), f$csv)
  expect_error(bs_review_confirm(f$plan, "i1", f$csv, list(ery="absent",nuclei="partial",vessel="unreviewed")), "new review revision")
  writeLines("class,x,y", f$csv)
  writeLines("tampered", file.path(f$plan$path,"receipts","000001.csv"))
  expect_error(bs_review_confirm(f$plan, "i1", f$csv, f$assertions), "new review revision")
  expect_false(dir.exists(file.path(f$plan$path, ".writer-lock")))
})

test_that("review scopes cannot contradict sparse points", {
  f <- review_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  for (cl in c("Spot_center", "Missed_area", "Review_uncertain")) {
    writeLines(c("class,x,y", paste0(cl,",4,5")), f$csv)
    expect_error(bs_review_confirm(f$plan,"i1",f$csv,f$assertions), "Absence assertion")
  }
  writeLines(c("class,x,y", "Cell_nucleus,4,5"), f$csv)
  expect_error(bs_review_confirm(f$plan,"i1",f$csv,f$assertions), "Unreviewed")
  writeLines(c("class,x,y", "False_positive,-1,5"), f$csv)
  expect_error(bs_review_confirm(f$plan,"i1",f$csv,f$assertions), "nonnegative")
  writeLines(c("class,x,y", "False_positive,4,5"), f$csv)
  expect_equal(bs_review_confirm(f$plan,"i1",f$csv,f$assertions)$n_points, 1L)
})

test_that("interrupted archives can resume only with identical saved contents", {
  f <- review_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  archive <- file.path(f$plan$path,"receipts","000001.csv")
  file.copy(f$csv, archive)
  expect_equal(bs_review_confirm(f$plan,"i1",f$csv,f$assertions)$status,"complete")
  writeLines("uncommitted different work",file.path(f$plan$path,"receipts","000002.csv"))
  expect_error(bs_review_confirm(f$plan,"i2",f$csv,f$assertions),"archive differs")
  expect_match(readLines(file.path(f$plan$path,"receipts","000002.csv")), "uncommitted")
})

test_that("review locks, changed images and malformed inputs fail without progress", {
  f <- review_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  dir.create(file.path(f$plan$path,".writer-lock"))
  expect_error(bs_review_confirm(f$plan,"i1",f$csv,f$assertions),"another writer")
  unlink(file.path(f$plan$path,".writer-lock"),recursive=TRUE)
  expect_error(bs_review_confirm(f$plan,NA_character_,f$csv,f$assertions),"Unknown")
  expect_error(bs_review_confirm(f$plan,"i1",NA_character_,f$assertions),"CSV")
  expect_error(bs_review_confirm(structure(list(path=NA_character_),class="bs_review_plan"),"i1",f$csv,f$assertions),"existing")
  writeLines("changed",f$images$path[1])
  expect_error(bs_review_confirm(f$plan,"i1",f$csv,f$assertions),"image changed")
  expect_length(list.files(file.path(f$plan$path,"receipts")),0)
})

test_that("queue creation respects budget and leaves RNG unchanged", {
  f <- review_fixture(); on.exit(unlink(f$root, recursive=TRUE))
  set.seed(876); before <- .Random.seed
  q <- bs_review_plan(f$images,budget=1,seed=4,out=file.path(f$root,"one"))
  expect_equal(nrow(q$items),1)
  expect_identical(.Random.seed,before)
  expect_error(bs_review_plan(f$images,budget=NA_real_,out=file.path(f$root,"bad")),"integers")
  expect_error(bs_review_plan(f$images,budget=0,out=file.path(f$root,"bad")),"positive")
  expect_error(bs_review_plan(f$images,out=f$plan$path),"new directory")
})

test_that("a fabricated earlier receipt cannot advance the review queue", {
  f <- review_fixture(); on.exit(unlink(f$root,recursive=TRUE))
  writeLines("{}",file.path(f$plan$path,"receipts","000001.json"))
  expect_error(bs_review_confirm(f$plan,"i2",f$csv,f$assertions),"Earlier review")
  expect_false(file.exists(file.path(f$plan$path,"receipts","000002.json")))
})
