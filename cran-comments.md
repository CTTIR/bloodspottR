## Submission

This is a new submission.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'R. Heller <raban.heller@uni-ulm.de>'
  New submission
  Suggests or Enhances not in mainstream repositories:
    cellspecR
  Availability using Additional_repositories specification:
    cellspecR   yes   https://cttir.r-universe.dev

  This is a first submission. 'cellspecR' is an optional companion package
  from the same group, available from the declared
  `Additional_repositories`. It is only used conditionally
  (`requireNamespace()` in `bs_as_cellspec()` and the vignette,
  `skip_if_not_installed()` in tests).

## Test environments

* Local: macOS (aarch64-apple-darwin), R 4.6.1,
  `R CMD check --as-cran --no-manual`. The PDF manual was built locally
  with TinyTeX (TeX Live 2026) via `R CMD Rd2pdf` without errors; the check
  run itself used `--no-manual`.
* GitHub Actions (r-lib/actions v2, `--as-cran`): macos-latest (release),
  windows-latest (release), ubuntu-latest (devel, release, oldrel-1), plus an
  informational ubuntu-latest oldrel-3 row.

## Notes

* The package bundles the minified 'OpenSeadragon' 6.1.1 viewer
  (BSD-3-Clause). Its copyright holders, license and upstream source are
  listed in `inst/COPYRIGHTS` and declared in the DESCRIPTION `Copyright`
  field.
* Examples, tests and vignettes use synthetic data, need no network access
  and write only below `tempdir()`. Examples that start an external worker
  use the running R installation's `Rscript` and are guarded by
  `requireNamespace("processx")`.
* No Python, slide reader or GPU software is used during checks.
