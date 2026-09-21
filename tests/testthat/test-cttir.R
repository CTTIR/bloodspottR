test_that("inventory is metadata-only and preserves duplicate identities", {
  root <- tempfile(); dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  for (d in c("one", "two", "bad", "resource")) dir.create(file.path(root, d))
  for (d in c("one", "two")) {
    writeLines(c("Package: example", "Version: 1.0", "Title: Example"), file.path(root, d, "DESCRIPTION"))
    writeLines(c("export(alpha, beta)", "base::stop('must not execute')"), file.path(root, d, "NAMESPACE"))
  }
  writeLines("invalid", file.path(root, "bad", "DESCRIPTION"))
  x <- bs_cttir_inventory(root, TRUE)
  expect_equal(nrow(x), 3)
  expect_equal(x$exports[x$directory == "one"], "alpha;beta")
  expect_equal(sum(x$duplicate_package), 2)
  expect_equal(x$status[x$directory == "bad"], "invalid_description")
  expect_true(all(is.na(x$installed_version)))
  expect_error(bs_cttir_inventory("/does-not-exist"), "existing directory")
  expect_error(bs_cttir_inventory(root, NA), "TRUE or FALSE")
  expect_equal(nrow(bs_cttir_inventory(file.path(root, "resource"))), 0)
})

test_that("inventory distinguishes installed metadata and missing namespace files", {
  root <- tempfile(); dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  for (d in c("stats", "unknown")) {
    dir.create(file.path(root, d))
    writeLines(c(paste0("Package: ", d), "Version: 0.0.0", "Title: Fixture"), file.path(root, d, "DESCRIPTION"))
  }
  writeLines("export(", file.path(root, "unknown", "NAMESPACE"))
  x <- bs_cttir_inventory(root, TRUE)
  expect_equal(x$installed_version[x$package == "stats"], as.character(utils::packageVersion("stats")))
  expect_true(is.na(x$installed_version[x$package == "unknown"]))
  expect_true(all(x$exports == ""))
})
