# bloodspottR 0.2.0

* The default import profile of `bs_import_legacy()` is now called `"legacy_areas"`, after what it does: it maps the legacy area columns `tissue_mm2` and `red_mm2` to `tissue_area_mm2` and `red_area_mm2`. The former name `"p079"` still works as a deprecated alias with identical results and signals a deprecation through 'lifecycle', which is now imported.
* The Overview plot of `bs_app()` uses the colour-vision-deficiency-safe Okabe-Ito colours together with symbols, titles its legend with `group_label`, places the legend above the plot instead of over the points, starts both axes at zero and draws text at the size of the surrounding page. In a narrow window with many groups the legend text shrinks and surplus entries are summarised as "+ n more", so the plot always draws.
* `bs_report()` shows non-integer values in its HTML tables with six significant digits and whole counts in full; the CSV and JSON files are unchanged. The Slides table of `bs_app()` shows whole counts without decimals.
* Add a package help topic (`?bloodspottR`), group the reference index of the documentation site by task, and link the README tutorials to the rendered articles.

* Malformed inputs now fail with catchable `bloodspottr_error` conditions instead of base R errors: a result object without a `slides` data frame in `bs_validate()` and `bs_summarize()`, unparseable or non-object JSON and unreadable CSV files in `bs_import_legacy()`, a matrix grouping column in `bs_summarize()`, unknown `profile`, `support` and `coordinate_unit` choices, and missing result or image-manifest files in `bs_app()`. Valid calls, partial matching of choices and defaults are unchanged.
* `bs_status()` no longer reports a measurement column named `source` as the import source of a plain slide table.
* `aggregate_burden()` reads an `n_profiles` column that is entirely logical `NA` as unknown profile counts, as the import functions already do, instead of rejecting it.

* Calibrated burden and pooled summaries reject dimensioned or classed numeric measurements before arithmetic. Matrix count/area columns can no longer multiply counts or denominators while leaving the measurement count unchanged; grouping IDs and quality flags also require one value per row. Valid calibration, exact-count limits and missing-profile propagation are unchanged.

* Prepare the first CRAN submission: every exported function has a runnable
  example, the description is phrased as full sentences, `Language: en-GB`,
  the documentation site URL and the bundled 'OpenSeadragon' copyright
  (`inst/COPYRIGHTS`) are declared, and a spelling wordlist is added.
* The help pages, README, vignettes and these notes use British spelling
  (colour, micrometre, artefact, analyse). Function names, operation names
  and JSON keys such as `bs_analyze()`, `"analyze"` and `artifacts` keep
  their spelling.
* `CITATION.cff` names the author as "R. Heller", as in DESCRIPTION.
* Deploy the pkgdown site with GitHub Pages from a new workflow.

* `bs_run_backend()`, `bs_train()`, `bs_analyze()` and native slide reads
  follow a running worker through its own process handle instead of listing
  the processes of the machine at every poll, which cost a short worker run
  15-20 seconds of system time on a busy Windows machine. Polls start at 50 ms
  and back off to 0.5 s; the worker's exit is still noticed at once. While the
  worker runs, its descendants are followed through the kernel's child lists on
  Linux, by a process scan on macOS (whose protected system programs hide their
  environment) and not at all on Windows. At clean-up a worker that is still
  running is scanned once for descendants before it is stopped, one sweep for
  the worker's environment marker finds helpers left behind, and every stopped
  process is verified. A descendant scan asks only processes started no
  earlier than the worker for their parent; a child that exits during the scan
  is skipped on its own, and a failure to list processes still stops the run.

* Track and verify observed worker descendants during timeout, failure and
  successful completion; isolate output streams from inherited pipes and bound
  receipt logs. Native slide reads use the same process handling.
* Preserve comparison run names and excluded physical-slide IDs in canonical
  deliveries and display comparison direction in HTML reports.
* Retain valid results when a saved refresh loses its configured grouping
  column, and reject annotation transforms that overflow numeric range.

* Standardise the named v5 package logo across README, documentation pages
  and available application headers, with matching light and dark artwork.


- Preserve exact tissue boundaries across platforms and distinguish exported table names during folder imports.

* Re-audit: exact count parsing and identity checks in Shiny, unambiguous filters,
  preserved comparison exports, and catchable `bloodspottr_error` conditions.
* Added preview/native whole-slide browsing, QuPath GeoJSON and point-CSV
  selection, saved-file refresh, arbitrary tissue labels and configurable grouping.
* Added a Shiny tutorial, responsive branded layout, completeness explanation,
  and shared CTTIR check, coverage and blocking house-lint workflows.

* Add validated legacy and canonical slide-result import, pooled burden summaries,
  and comparisons that require matching measured support.
* Add immutable delivery directories with CSV, JSON, optional XLSX, HTML reports,
  and SHA-256 manifests.
* Add a Shiny results explorer with explicit synthetic demonstration data,
  organ and quality filters, missing-data visibility, and guarded uploads.
* Add a bounded review queue with saved-export receipts and class-specific scope.
* Add a versioned external-worker protocol with timeout clean-up and input/output
  verification; native DNN workers remain separately qualified integrations.
* Add source-suite inventory and calibrated candidate exchange with cellspecR.
* Preserve the existing calibrated burden and spatial APIs.
* Add relative OD/chromaticity features and calibrated directional/profile
  diameter flags, with no area-based cell-count inference.
