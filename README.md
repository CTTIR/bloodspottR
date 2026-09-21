# bloodspottR <img src="inst/figures/bloodspottR_icon_600dpi.png" align="right" height="160" alt="bloodspottR hex sticker" />

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/CTTIR/bloodspottR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CTTIR/bloodspottR/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE.md)

**Calibrated histology measurements, traceable review and transparent reporting.**

bloodspottR brings red-positive tissue area and image-derived event candidates
into an explicit physical-slide workflow. It preserves measurement support,
missing values, quality decisions and provenance while keeping image readers,
annotation clients and model execution behind defined interfaces.

**Experimental software.** Numerical and software checks do not establish
biological accuracy. A candidate event is not necessarily one erythrocyte.
Native whole-slide inference, QuPath project writeback and portable DNN training
are not yet qualified end-to-end package features. The preserved study pipeline
and the reusable R package have separate validation histories.

## Installation

```r
install.packages("remotes")
remotes::install_github("CTTIR/bloodspottR")
install.packages("shiny")  # Optional results application
```

Required runtime dependencies are **cli**, **digest** and **jsonlite**.
R 4.2 or newer is required. The package itself has no compiled code; optional
spatial dependencies may need system libraries on Linux. To build the tutorials,
install **knitr** and **rmarkdown**, make Pandoc available (included with RStudio),
then reinstall with `build_vignettes = TRUE`. Install
**openxlsx2** for workbook export. For the optional candidate-exchange adapter,
install **cellspecR** from `CTTIR/cellspecR`. No GPU software is needed to explore
existing results. The package is a development version and is not on CRAN.

## Start with an explicit demonstration

```r
library(bloodspottR)

results <- bs_example()  # Deterministic synthetic data, never study results
bs_validate(results)
results$groups <- bs_summarize(results, strata = c("organ", "stratum"))
results$groups

# Local read-only explorer; no inference service starts.
if (interactive()) bs_app(results)
```

The demonstration deliberately includes missing values and zero tissue support.
Pooled summaries propagate missing measurements. A zero numerator over known
positive tissue gives a measured zero; an unknown or zero denominator gives a
missing rate. Group rates divide sums, rather than averaging slide percentages.

## Browse annotated slides

The Shiny app displays existing slide images, annotations and measurements. It
supports arbitrary organ labels and configurable grouping metadata. Use the
synthetic four-tissue example to try image navigation, zoom/pan and annotation
selection:

```r
demo <- bs_viewer_example()
if (interactive()) bs_app(demo$results, images = demo$images,
                          title = "My slide collection")
# When the app has stopped:
unlink(demo$directory, recursive = TRUE)
```

![Annotated-slide browser with synthetic data and a selected annotation](inst/figures/shiny-demo.png)

For your data, pass an image manifest with `image_id`, `slide_id`, `path` and
optional `annotations` and `label` columns. PNG/JPEG previews and saved overlays
work directly. Native whole-slide images use an optional OpenSlide Python reader;
QuPath GeoJSON supplies selectable points and polygons with their recorded
measurements. Multiple images or regions may link to one slide. `group_by` and
`group_label` customize the tissue filter; `title` customizes the heading.

Passing a result file path enables refresh of saved measurements; Watch saved
files also refreshes exported annotations. It never edits the source project or
reruns analysis. See the [viewer tutorial](vignettes/shiny-results-explorer.Rmd)
for coordinate transforms, Python setup, format limits and complete examples.

## Import, summarize and preserve a delivery

```r
# Adapt a preserved study summary, or use profile = "canonical" for a canonical CSV.
# results <- bs_import_legacy("path/to/analysis.json")

results <- bs_example()
results$groups <- bs_summarize(results, strata = "organ")

# New destinations only: existing deliveries are never overwritten.
delivery <- tempfile("bloodspottr-report-")
report <- bs_report(
  results, delivery, author = "RH",
  background = "Synthetic demonstration of calibrated slide reporting."
)
report
list.files(delivery)
```

`bs_report()` writes self-contained HTML, CSV tables, canonical JSON and a
SHA-256 manifest. Supplied comparisons also receive a `Comparison.csv` table.
`bs_export_results()` writes the table bundle without HTML;
`xlsx = TRUE` adds a workbook when **openxlsx2** is installed. Large images,
individual annotation geometry and training weights remain separate artifacts.
The initial package renderer does not recreate the study-specific TeX/PDF image
supplement; its HTML can be printed from a browser.

## What is available

| Area | Implemented interface |
| --- | --- |
| Project metadata | `bs_project()`, `bs_status()` |
| Preserved measurements | `bs_import_legacy()`, `bs_validate()`, `bs_summarize()` |
| Paired comparison | `bs_compare()` with explicit shared-support checks |
| Review records | Saved-artifact review plans and confirmation receipts |
| Backend boundary | Versioned requests and explicit executable invocation |
| Suite reuse | `bs_cttir_inventory()`, calibrated `bs_as_cellspec()` exchange |
| Results | `bs_export_results()`, `bs_report()`, `bs_app()` |
| Existing measurement core | `measure_burden()`, `aggregate_burden()`, `measure_event_burden()`, `aggregate_event_burden()` |
| Color and profile shape | `bs_color_features()`, `bs_profile_extent()` |
| Optional spatial geometry | `tissue_window()`, `cell_pattern()` |

The package inventories source metadata across a chosen CTTIR suite directory
without loading or installing those packages. Runtime compatibility is tested
for the implemented cellspecR exchange; other adapters require qualification
within their respective scopes.

## Documentation

After installation with vignettes:

```r
vignette("bloodspottr-workflow", package = "bloodspottR")
vignette("cttir-interoperability", package = "bloodspottR")
vignette("review-and-backends", package = "bloodspottR")
vignette("shiny-results-explorer", package = "bloodspottR")
```

| Tutorial | Start here when you want to… |
| --- | --- |
| [Measurements to a report](vignettes/bloodspottr-workflow.Rmd) | Import, pool, compare and export slide measurements |
| [Shiny results explorer](vignettes/shiny-results-explorer.Rmd) | Upload data, filter slides and interpret missing values |
| [CTTIR interoperability](vignettes/cttir-interoperability.Rmd) | Exchange calibrated candidates with cellspecR |
| [Review and backends](vignettes/review-and-backends.Rmd) | Record saved reviews and invoke a trusted external worker |

The vignettes use runnable synthetic examples and state each interface's limits. Function
help documents the implemented interfaces. Example data are synthetic.

## Validation and scope

The test suite covers exact-count boundaries, missing support, pooled rates,
transactional exports, review receipts, worker failures and calibrated exchange.
CI calls the shared CTTIR package-check, coverage and house-lint workflows.
Check the linked workflow results for the current commit; a configured workflow
alone does not establish cross-platform qualification. Software checks and visual
inspection do not establish biological accuracy.

The Shiny application inspects recorded results, preview images and OpenSlide-supported
whole-slide images. Native inference, portable DNN training, QuPath project
writeback and generic slide image PDF supplements remain separate work.
Configured external workers are trusted code, not a security sandbox. Missing
measurements remain unknown, and low-signal slides are not inferred controls.

## Input contracts and errors

Slide IDs are nonempty character strings; keep leading zeros. Areas use mm² and
densities use counts/mm². The explicit unit suffixes in the existing measurement
API are retained for compatibility. Missing measurements use `NA`; zero denotes
an observed zero. Counts must be whole and at most `2^53`, including pooled
counts. Use canonical `results.json` for lossless metadata exchange; presentation
CSVs escape formula-like text for spreadsheet safety.

Package validation failures inherit from `bloodspottr_error`, so callers can
handle them with `tryCatch(..., bloodspottr_error = function(e) ...)`. Errors from
external dependencies may retain their own classes. Report reproducible issues
through [GitHub issues](https://github.com/CTTIR/bloodspottR/issues), using synthetic
or de-identified examples.

## License

MIT. Copyright 2026 R. Heller. See [LICENSE.md](LICENSE.md).
The bundled OpenSeadragon viewer retains its [BSD-3-Clause license](inst/app/vendor/LICENSE-openseadragon.txt).
OpenSlide is an optional external dependency and is not bundled.
