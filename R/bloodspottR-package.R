#' bloodspottR: Calibrated Histology Burden and Erythrocyte Profile Analysis
#'
#' Physically calibrated histology burden summaries with explicit
#' measurement-area accounting, pooled physical-slide results, traceable review
#' and delivery records, and a local Shiny results explorer. Image-derived
#' candidates are exploratory and require independent validation before
#' biological interpretation.
#'
#' Validation failures raised by the package inherit from the condition class
#' `bloodspottr_error`.
#'
#' @section Main functions:
#' * Measurement: [measure_burden()], [aggregate_burden()],
#'   [measure_event_burden()], [aggregate_event_burden()].
#' * Color and profile shape: [bs_color_features()], [bs_profile_extent()].
#' * Spatial geometry: [tissue_window()], [cell_pattern()].
#' * Results: [bs_import_legacy()], [bs_validate()], [bs_summarize()],
#'   [bs_compare()], [bs_project()], [bs_status()].
#' * Delivery: [bs_export_results()], [bs_report()].
#' * Review records: [bs_review_plan()], [bs_review_confirm()].
#' * External workers: [bs_backend()], [bs_run_backend()], [bs_train()],
#'   [bs_analyze()].
#' * Application and examples: [bs_app()], [bs_viewer_example()], [bs_example()].
#' * Suite interoperability: [bs_as_cellspec()], [bs_cttir_inventory()].
#'
#' Start with `vignette("bloodspottr-workflow", package = "bloodspottR")`.
#'
#' @keywords internal
"_PACKAGE"
