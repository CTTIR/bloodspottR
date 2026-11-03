# Maintenance audit — 2026-10-30

Scope: development R package 0.1.0.9000, baseline
`c8ff49f228097bf716eed1c3e092cc187b886ca4`. Work takes place in an isolated
checkout. The original checkout, study data and other active projects are preserved.

## Purpose and acceptance criteria

Dependable calibrated histology summaries, inspectable saved images/annotations,
and reproducible review, worker and delivery receipts. Counts remain exploratory
candidates; software validation cannot establish biological accuracy. Existing
area, exact-count, missing-value and support-identity contracts remain mandatory.

1. Worker success, failure and timeout leave no observed owned process running;
   unrelated jobs survive. No incomplete output is published.
2. Export/import preserves comparison direction and excluded-slide provenance.
3. Invalid viewer refreshes preserve the last valid result with an explicit error;
   invalid transformed annotation coordinates are rejected.
4. Optional spatial, cellspecR and workbook workflows pass with dependencies
   installed. Full package checks, source installation, documentation and first
   use are exercised and their actual results recorded.

## Findings and roadmap

| Priority | Evidence and impact | Action and validation |
| --- | --- | --- |
| Essential | Existing `test-backend.R` timeout test fails locally. Five direct processx 3.9.0 probes leave a child `sleep` alive. Marker lookup finds the R worker but not this descendant on macOS. | Track process ancestry while the worker is alive; retain process handles, terminate and verify observed descendants before publication. Test timeout, success, failure and unrelated process survival. |
| Essential | `bs_compare()` stores run names and excluded IDs in R attributes; JSON export silently drops both. Reimport loses the meaning of first/second and exclusions. | Serialize comparison context explicitly and restore it; expose it in reports. Verify round trips including empty and numeric-looking IDs. |
| Everyday use | Saved-data refresh validates measurements but does not require the configured grouping column. Coordinate transforms can overflow after valid finite input. | Validate before replacing results; reject nonfinite transformed geometry. Regression tests at the affected boundaries. |
| Release qualification | Six baseline skips cover optional packages absent from this host; only pre-existing commits have successful hosted CI. | Install optional dependencies in an isolated library, run complete checks and exercise exported products. Record hosted verification separately. |
| Deferred | Native model training/inference, QuPath writeback, biological validation and production scale are explicitly unqualified in existing documentation. | Require independent datasets, supported native workers and deployment-specific acceptance work; do not expand this maintenance release into those features. |

Worker execution is trusted code, not an OS sandbox. Workers must keep helpers
within their process ancestry until observed and must join them before successful
exit. An intentionally detached process that becomes unidentifiable between
observations cannot be guaranteed recoverable. Process cleanup will not kill
unrelated jobs to compensate for that limitation.

Current upstream references: [processx cleanup](https://processx.r-lib.org/articles/cleanup.html)
and [process API](https://processx.r-lib.org/reference/process.html). The documented
marker mechanism was checked against installed code and direct execution; local
behavior, rather than the documentation claim alone, determines qualification.

## Completed work and verification

All four local acceptance criteria above are satisfied. The worker runner combines
processx marker cleanup with observed ancestry handles, redirects output to files
to avoid inherited-pipe hangs, checks log sizes during execution, and verifies
observed descendants before committing output. Receipt logs exceeding 10 MiB are
rejected; polling is not a hard operating-system disk quota. The native slide
adapter shares this runner and reports timeouts explicitly. Comparison JSON now
contains `comparison_context`; imports restore attributes and reports show run
direction/exclusions. Viewer validation occurs before replacing current data.

Baseline: one confirmed failure in owned-child timeout cleanup and six missing
optional-dependency skips. No GitHub releases or open issues were returned by the
authenticated read. Verification after repairs on macOS 27.0.1 / ARM64 / R 4.6.1:

- 87 tests, **586 passing assertions**, zero failures, errors, warnings or skips.
  Real workers cover success, nonzero exit, timeout, inherited output streams,
  literal arguments, excessive output and preservation of unrelated processes.
- `covr::package_coverage()`: **95.59%**, above the configured 95% floor.
- Full `R CMD check --as-cran`: **0 errors, 0 warnings, 1 NOTE**, including rebuilt
  vignettes, indexed PDF and HTML manuals, examples and all tests. The note covers
  new submission, the development version, `Remotes`, and the optional cellspecR
  dependency outside mainstream repositories. This is not a CRAN-ready submission.
- Source archive installed into a fresh package library. Core first use, grouped
  summaries, report generation, checksum readback and comparison round trip passed
  without loading the optional worker, spreadsheet or spatial packages. Other
  installed core R dependencies were available; this was not a virgin OS test.
- Blocking CTTIR house lint: zero findings. Workflow actionlint passed with
  shellcheck and pyflakes unavailable/disabled. `git diff --check` passed.
- pkgdown: 39 HTML pages, 1,361 local references checked, no missing local files.
  Upstream Pandoc emits a deprecated math-option warning; rendering succeeds.
- Chromium 156.0.8078.4 browser checks: empty startup, explicit synthetic demo,
  12 annotations, selection/properties, zoom, visibility, slide navigation/search,
  filtered CSV download, invalid-upload recovery, and 390px mobile layout.
  No page errors or failed HTTP responses. Desktop/mobile and comparison-report
  renderings were opened and visually inspected.
- Real OpenSlide metadata and tile extraction from the upstream Aperio
  [CMU-1-Small-Region.svs fixture](https://openslide.cs.cmu.edu/download/openslide-testdata/Aperio/CMU-1-Small-Region.svs):
  2220 × 2967 pixels; valid tile returned PNG; invalid level/tile rejected.
  Source SHA-256: `ed92d5a9f2e86df67640d6f92ce3e231419ce127131697fbbce42ad5e002c8a7`.
  The fixture and Python environment remain outside the package. Other slide
  vendors and large-slide performance are not qualified by this small fixture.

Dependency versions: processx 3.9.0, ps 1.9.3, sf 1.1.3, spatstat.geom 3.8.3,
openxlsx2 1.30, cellspecR 1.1.0 at `bd940f1baf937cd6a78f8741193ac419d34c712b`,
Shiny 1.14.0, testthat 3.3.2, pkgdown 2.2.1; OpenSlide Python 1.4.7,
OpenSlide binary 4.0.1.2 and Pillow 13.0.0 in a separate Python 3.15 environment.

Machine evidence is retained outside source control under
`/Users/rh/Documents/GitHub/.repository-audits/cttir-public-20261030/evidence/bloodspottR/`,
including final test/coverage/check/lint logs, source tarball, dependency logs,
browser scripts and receipts, screenshots, native tile evidence and first-use
delivery. The original checkout and remote main remained at the baseline.

## Repeat installation and first use

Prerequisites: R >= 4.2, R package build tools, network access to CRAN for core
dependencies; Pandoc for rebuilding vignettes and LaTeX for the PDF check.
From this checkout:

```sh
Rscript --vanilla -e 'install.packages(c("cli", "digest", "jsonlite", "knitr", "rmarkdown"), repos="https://cloud.r-project.org")'
R CMD build .
R CMD INSTALL bloodspottR_0.1.0.9000.tar.gz
```

```r
library(bloodspottR)
x <- bs_example()
bs_validate(x)
x$groups <- bs_summarize(x, c("organ", "stratum"))
delivery <- tempfile("bloodspottr-report-")
bs_report(x, delivery, author = "R. Heller")
restored <- bs_import_legacy(delivery)
# Optional: install.packages("shiny")
bs_app(x) # starts interactively; use launch = FALSE to construct only
```

For full checks, install DESCRIPTION Suggests, including cellspecR from its
declared GitHub remote, and use:

```sh
Rscript --vanilla -e 'testthat::test_local()'
Rscript --vanilla -e 'covr::package_coverage()'
R CMD check --as-cran bloodspottR_0.1.0.9000.tar.gz
Rscript --vanilla -e 'pkgdown::build_site(preview = FALSE)'
```

## Remaining gates and next action

The original audit prepared local changes; its later source publication did not
constitute a release, deployment or submission. Observe the hosted R/OS matrix
and coverage jobs for each subsequent revision. For a CRAN target, first choose a release version and
resolve/discuss the cellspecR availability and Remotes note, then rerun the checks
on that exact submission archive. Existing successful hosted runs predate these
changes. Biological validation, detached workers, native model training/inference,
QuPath writeback, vendor breadth and production-scale benchmarks remain outside
this development release, as described above.

## Linux process follow-up — 3 November 2026

Hosted checks exposed a test that reacquired a child handle after the child had
been reaped. With Linux ps 1.9.3 this raises `os_error`/ENOENT before an identity
handle exists, rather than the `no_such_process` condition expected by the test.
The fixture now records PID and creation time while the child is alive and uses
that identity for the existing survivor checks. It never treats an unrelated
process reusing the PID as the original child.

Real repeated worker runs then exposed the same snapshot-to-handle race inside
recursive descendant enumeration. That operation now retries a fresh snapshot
at most three times for ENOENT only. Previously observed handles are retained;
persistent ENOENT and other errors still fail. No failure is converted to an
empty, successful cleanup. The original ps condition is rethrown unchanged.

Validation used a disposable Ubuntu 24.04 aarch64 container with R 4.3.3,
ps 1.9.3 and processx 3.9.0. The unchanged baseline failed; the repair passed
five repetitions of all 24 maintenance assertions, including successful and
failing workers, inherited streams and unrelated-process preservation. Eleven
new assertions cover a disappearing child, repeated ENOENT on a live parent and
a permission error. Independent review reran those eleven assertions.
The macOS package suite and as-CRAN check passed (zero errors/warnings, one
existing incoming-feasibility note); instrumented coverage was 95.55416%, above
the existing 95% gate. Exact follow-up logs and source identities are under
`hosted-followup/bloodspottR/runtime` in the external maintenance evidence.
Hosted checks for the follow-up commit must still be observed separately.
