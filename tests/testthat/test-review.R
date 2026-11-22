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

test_that("queue creation rejects incomplete, duplicated and missing image records", {
  f <- review_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  out <- file.path(f$root, "rejected")
  expect_error(bs_review_plan(as.list(f$images), out = out), "item_id, parent_id and path")
  expect_error(bs_review_plan(f$images[0, ], out = out), "item_id, parent_id and path")
  expect_error(bs_review_plan(f$images[c("item_id", "path")], out = out),
               "item_id, parent_id and path")
  missing_id <- f$images; missing_id$item_id[2] <- NA
  expect_error(bs_review_plan(missing_id, out = out), "cannot be missing")
  blank_parent <- f$images; blank_parent$parent_id <- "  "
  expect_error(bs_review_plan(blank_parent, out = out), "cannot be missing")
  duplicated_id <- f$images; duplicated_id$item_id <- "i1"
  expect_error(bs_review_plan(duplicated_id, out = out), "must be unique")
  absent <- f$images; absent$path[2] <- file.path(f$root, "absent.bin")
  expect_error(bs_review_plan(absent, out = out), "existing files")
  folder <- f$images; folder$path[1] <- f$root
  expect_error(bs_review_plan(folder, out = out), "existing files")
  expect_error(bs_review_plan(f$images, seed = -1, out = out), "nonnegative integers")
  expect_error(bs_review_plan(f$images, budget = 1.5, out = out), "nonnegative integers")
  expect_error(bs_review_plan(f$images, out = file.path(f$root, "no", "parent")), "new directory")
  expect_false(file.exists(out))
  expect_length(list.files(f$root, pattern = "bloodspottr-review", all.files = TRUE), 0)
})

test_that("queue creation does not initialise the random-number state", {
  f <- review_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) saved <- get(".Random.seed", envir = globalenv())
  on.exit(if (had_seed) assign(".Random.seed", saved, envir = globalenv()), add = TRUE)
  if (had_seed) rm(".Random.seed", envir = globalenv())
  first <- bs_review_plan(f$images, budget = 1, seed = 9, out = file.path(f$root, "first"))
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  second <- bs_review_plan(f$images, budget = 1, seed = 9, out = file.path(f$root, "second"))
  expect_identical(first$items, second$items)
  expect_identical(unname(first$items$sha256),
                   digest::digest(file = f$images$path[1], algo = "sha256"))
})

test_that("damaged review manifests stop confirmation and release the writer lock", {
  f <- review_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  manifest <- file.path(f$plan$path, "plan.json")
  original <- readLines(manifest)
  rewrite <- function(change) {
    stored <- jsonlite::read_json(manifest, simplifyVector = TRUE)
    jsonlite::write_json(change(stored), manifest, auto_unbox = TRUE, dataframe = "rows")
  }
  damage <- list(
    function(x) { x$schema_version <- 2L; x },
    function(x) { x$items <- "none"; x },
    function(x) { x$items$sha256 <- NULL; x },
    function(x) { x$items$item_id <- "i1"; x },
    function(x) { x$items$parent_id <- ""; x })
  for (change in damage) {
    rewrite(change)
    expect_error(bs_review_confirm(f$plan, "i1", f$csv, f$assertions), "Invalid review manifest")
    expect_false(dir.exists(file.path(f$plan$path, ".writer-lock")))
    writeLines(original, manifest)
  }
  unlink(file.path(f$plan$path, "receipts"), recursive = TRUE)
  expect_error(bs_review_confirm(f$plan, "i1", f$csv, f$assertions), "Invalid review manifest")
  dir.create(file.path(f$plan$path, "receipts"))
  expect_equal(bs_review_confirm(f$plan, "i1", f$csv, f$assertions)$status, "complete")
})

test_that("annotation exports need known classes, numeric coordinates and valid scopes", {
  f <- review_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  reviewed <- list(ery = "partial", nuclei = "unreviewed", vessel = "unreviewed")
  malformed <- list(c("label,x,y", "Spot_center,4,5"), c("class,x", "Spot_center,4"),
                    c("class,x,y,x", "Spot_center,4,5,6"), c("class,x,y", "Platelet,4,5"),
                    c("class,x,y", ",4,5"), c("class,x,y", "Spot_center,left,5"),
                    c("class,x,y", "Spot_center,4,top"))
  for (lines in malformed) {
    writeLines(lines, f$csv)
    expect_error(bs_review_confirm(f$plan, "i1", f$csv, reviewed),
                 "Invalid annotation columns or classes")
  }
  writeLines(c("class,x,y", "Spot_center,4,5", "Spot_center,4,"), f$csv)
  expect_error(bs_review_confirm(f$plan, "i1", f$csv, reviewed), "finite and nonnegative")
  writeLines(c("class,x,y", "Spot_center,4,5"), f$csv)
  for (assertions in list(reviewed[1:2], c(reviewed, list(other = "partial")),
                          list(ery = "done", nuclei = "unreviewed", vessel = "unreviewed"),
                          list(ery = c("partial", "complete"), nuclei = "absent", vessel = "absent"),
                          unlist(reviewed))) {
    expect_error(bs_review_confirm(f$plan, "i1", f$csv, assertions), "review assertions required")
  }
  expect_error(bs_review_confirm(f$plan, "i1", f$root, reviewed), "CSV required")
  expect_error(bs_review_confirm(f$plan, c("i1", "i2"), f$csv, reviewed), "Unknown review item")
  expect_length(list.files(file.path(f$plan$path, "receipts")), 0)
  shuffled <- reviewed[c("vessel", "ery", "nuclei")]
  receipt <- bs_review_confirm(f$plan, "i1", f$csv, shuffled)
  expect_identical(names(receipt$assertions), c("ery", "nuclei", "vessel"))
  expect_equal(receipt$n_points, 1L)
})
