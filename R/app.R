#' Explore slide results in a local Shiny application
#'
#' Presents imported measurements without altering scientific values. CSV uploads
#' contain a slide table; JSON uploads contain a result object with a `slides`
#' table. Uploaded files are parsed as data, never evaluated as R code.
#' @param results A result list with a `slides` data frame, or `NULL`.
#' @param launch Launch the application; `FALSE` returns a Shiny application.
#' @param images Optional data frame or CSV manifest with `image_id`, `slide_id`,
#'   `path`, and optional `label`, `annotations`, `scale_x`, `scale_y`, `offset_x`,
#'   `offset_y`. Paths in CSV manifests are relative to the manifest. Images are
#'   PNG/JPEG previews or OpenSlide-supported native slides. Annotations are
#'   QuPath GeoJSON or point CSV with x/y image-pixel coordinates. Transforms map
#'   annotation coordinates into the displayed image: pixel * scale + offset.
#' @param title Application title.
#' @param group_by Metadata column used for grouping and filtering, default organ.
#' @param group_label Display label for this grouping, default Tissue.
#' @param slide_python Python executable with openslide-python and openslide-bin
#'   for native slides; unused for PNG/JPEG previews. No packages are installed.
#' @details `results` can also be a saved result file path. The viewer's optional
#'   watch mode rereads this file and saved annotations every two seconds; failed
#'   reads retain the previous measurements. It never changes source files.
#'   Native QuPath project files and TissueGnostics acquisition projects need
#'   image/GeoJSON exports; they are not themselves OpenSlide image formats.
#' @return A Shiny application object, invisibly when launched.
#' @export
bs_app <- function(results = NULL, launch = interactive(), images = NULL,
                   title = "bloodspottR", group_by = "organ", group_label = "Tissue",
                   slide_python = Sys.getenv("BLOODSPOTTR_PYTHON", "")) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    .bs_abort("Install the optional 'shiny' package to use bs_app().", call. = FALSE)
  }
  if (!is.logical(launch) || length(launch) != 1L || is.na(launch)) {
    .bs_abort("launch must be TRUE or FALSE.", call. = FALSE)
  }
  for (value in list(title, group_by, group_label)) .bs_scalar_string(value, "App labels")
  result_path <- if (is.character(results) && length(results) == 1L)
    normalizePath(results, mustWork = TRUE) else NULL
  if (!is.null(result_path)) results <- bs_import_legacy(result_path)
  if (!is.null(results)) results <- .bs_app_results(results)
  if (!is.null(results) && !group_by %in% names(results$slides))
    .bs_abort("group_by must name a result metadata column")
  image_table <- .bs_image_manifest(images, results$slides$slide_id)
  app <- shiny::shinyApp(.bs_app_ui(title, group_label, nrow(image_table) > 0L),
    .bs_app_server(results, image_table, slide_python, result_path, group_by),
    onStart = .bs_app_start)
  if (launch) shiny::runApp(app) else app
}

.bs_app_results <- function(x) {
  if (!is.list(x) || !is.data.frame(x$slides)) {
    .bs_abort("Results must contain a slides data frame.", call. = FALSE)
  }
  d <- x$slides
  if (anyDuplicated(names(d))) .bs_abort("Slide columns must have unique names.", call. = FALSE)
  if (!is.null(x$schema_version) && !identical(x$schema_version, "1.0")) {
    .bs_abort("Unsupported result schema.")
  }
  if (all(c("slide_id", "physical_slide_id") %in% names(d)) &&
      !identical(d$slide_id, d$physical_slide_id)) {
    .bs_abort("slide_id and physical_slide_id must agree.")
  }
  if (!"slide_id" %in% names(d) && "physical_slide_id" %in% names(d)) d$slide_id <- d$physical_slide_id
  if (!"qc_stratum" %in% names(d) && "stratum" %in% names(d)) d$qc_stratum <- d$stratum
  required <- c("slide_id", "tissue_area_mm2", "red_area_mm2", "candidate_spots")
  missing <- setdiff(required, names(d))
  if (length(missing)) .bs_abort("Missing slide columns: ", paste(missing, collapse = ", "), call. = FALSE)
  if (!is.character(d$slide_id) || anyNA(d$slide_id) || any(!nzchar(trimws(d$slide_id))) || anyDuplicated(d$slide_id)) {
    .bs_abort("Slide IDs must be nonempty, unique character strings.", call. = FALSE)
  }
  numeric_fields <- c(required[-1L], intersect(c("unresolved_bulk_events", "operational_events"), names(d)))
  for (name in numeric_fields) {
    if (is.logical(d[[name]]) && all(is.na(d[[name]]))) d[[name]] <- as.numeric(d[[name]])
    if (is.character(d[[name]]) && name %in% .bs_metrics[3:5]) {
      d[[name]] <- .bs_parse_count_text(d[[name]], name)
    }
    v <- d[[name]]
    if (!is.numeric(v) || any(is.nan(v)) || any(!is.na(v) & (!is.finite(v) | v < 0))) {
      .bs_abort(name, " must contain nonnegative finite numbers or NA.", call. = FALSE)
    }
  }
  if (any(d$red_area_mm2 > d$tissue_area_mm2, na.rm = TRUE)) {
    .bs_abort("Red area cannot exceed tissue area.", call. = FALSE)
  }
  for (name in intersect(c("candidate_spots", "unresolved_bulk_events", "operational_events"), names(d))) {
    if (any(d[[name]] != floor(d[[name]]) | d[[name]] > 2^53, na.rm = TRUE)) {
      .bs_abort("Event counts must be exactly representable whole counts.", call. = FALSE)
    }
  }
  if (all(c("unresolved_bulk_events", "operational_events") %in% names(d))) {
    if (any(d$candidate_spots > 2^53 - d$unresolved_bulk_events, na.rm = TRUE))
      .bs_abort("Combined event counts exceed exact double precision.", call. = FALSE)
    if (any(d$operational_events != d$candidate_spots + d$unresolved_bulk_events, na.rm = TRUE))
      .bs_abort("Operational events must equal spots plus bulk events.", call. = FALSE)
  }
  any_count <- d$candidate_spots > 0
  if ("operational_events" %in% names(d)) any_count <- any_count | d$operational_events > 0
  if (any(d$tissue_area_mm2 == 0 & any_count, na.rm = TRUE)) {
    .bs_abort("Positive counts require positive tissue support.", call. = FALSE)
  }
  den <- ifelse(!is.na(d$tissue_area_mm2) & d$tissue_area_mm2 > 0,
                d$tissue_area_mm2, NA_real_)
  d$red_percent <- 100 * (d$red_area_mm2 / den)
  d$spots_per_mm2 <- d$candidate_spots / den
  if (any(is.infinite(d$red_percent) | is.infinite(d$spots_per_mm2))) {
    .bs_abort("Derived rates exceed finite numeric range.")
  }
  if ("operational_events" %in% names(d)) {
    d$events_per_mm2 <- d$operational_events / den
    if (any(is.infinite(d$events_per_mm2))) .bs_abort("Derived rates exceed finite numeric range.")
  }
  if (!"organ" %in% names(d)) d$organ <- rep("Unspecified", nrow(d))
  if (!"qc_stratum" %in% names(d)) d$qc_stratum <- rep("Unspecified", nrow(d))
  for (name in c("organ", "qc_stratum")) {
    d[[name]] <- as.character(d[[name]])
    d[[name]][is.na(d[[name]]) | !nzchar(d[[name]])] <- "Unspecified"
  }
  x$slides <- d
  x
}

.bs_app_read <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (!ext %in% c("csv", "json")) .bs_abort("Choose a CSV or JSON data file.", call. = FALSE)
  resolved <- normalizePath(path, winslash = "/", mustWork = FALSE)
  upload_root <- paste0(normalizePath(tempdir(), winslash = "/", mustWork = TRUE), "/")
  if (!startsWith(resolved, upload_root)) .bs_abort("Upload must be a session temporary file.", call. = FALSE)
  size <- file.info(path)$size
  if (is.na(size) || size > 20 * 1024^2) .bs_abort("Upload must be readable and at most 20 MiB.", call. = FALSE)
  if (ext == "csv") {
    d <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                         colClasses = "character", na.strings = "")
    for (n in intersect(c("tissue_area_mm2", "red_area_mm2", "candidate_spots",
                          "unresolved_bulk_events", "operational_events", "red_percent",
                          "spots_per_mm2", "events_per_mm2"), names(d))) {
      raw <- d[[n]]
      raw[!is.na(raw) & raw == "NA"] <- NA_character_
      d[[n]] <- if (n %in% .bs_metrics[3:5]) .bs_parse_count_text(raw, n) else
        suppressWarnings(as.numeric(raw))
      if (any(!is.na(raw) & nzchar(raw) & is.na(d[[n]]))) .bs_abort("Invalid numeric column: ", n, call. = FALSE)
    }
    x <- list(slides = d, provenance = list(source = basename(name)))
  } else {
    if (!requireNamespace("jsonlite", quietly = TRUE)) .bs_abort("Install jsonlite to read JSON.", call. = FALSE)
    x <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE, bigint_as_char = TRUE)
  }
  .bs_app_results(x)
}

.bs_app_metrics <- function(d) {
  # Missing support remains missing, including when only some slides are missing.
  tissue <- sum(d$tissue_area_mm2)
  red <- sum(d$red_area_mm2)
  if (!anyNA(d$candidate_spots)) {
    remaining <- 2^53
    for (v in sort(d$candidate_spots, decreasing = TRUE)) {
      if (v > remaining) .bs_abort("Pooled counts exceed exact double precision.", call. = FALSE)
      remaining <- remaining - v
    }
  }
  spots <- sum(d$candidate_spots)
  if (any(is.infinite(c(tissue, red)))) .bs_abort("Pooled measurements exceed finite numeric range.", call. = FALSE)
  values <- c(slides = nrow(d), tissue = tissue,
    red_percent = if (is.finite(tissue) && tissue > 0) 100 * (red / tissue) else NA_real_,
    spots_per_mm2 = if (is.finite(tissue) && tissue > 0) spots / tissue else NA_real_)
  if (any(is.infinite(values))) .bs_abort("Pooled rates exceed finite numeric range.", call. = FALSE)
  values
}

.bs_app_csv <- function(d, file) {
  d <- .bs_flat_table(d)
  # Avoid spreadsheet formula interpretation of externally supplied text.
  for (n in names(d)) if (is.character(d[[n]])) {
    hit <- !is.na(d[[n]]) & grepl("^[[:space:]]*[=+@-]", d[[n]])
    d[[n]][hit] <- paste0("'", d[[n]][hit])
  }
  utils::write.csv(d, file, row.names = FALSE, na = "")
}

.bs_app_ui <- function(title = "bloodspottR", group_label = "Tissue", has_images = FALSE) {
  css_path <- system.file("app", "style.css", package = "bloodspottR")
  css <- if (nzchar(css_path)) paste(readLines(css_path, warn = FALSE), collapse = "\n") else ""
  assets <- system.file("app", package = "bloodspottR")
  shiny::addResourcePath("bloodspottr-assets", assets)
  shiny::fluidPage(title = paste0(title, " | Slide results"), lang = "en",
    shiny::tags$head(shiny::tags$style(shiny::HTML(css)),
                    shiny::tags$script(src = "bloodspottr-assets/vendor/openseadragon.min.js"),
                    shiny::tags$script(src = "bloodspottr-assets/viewer.js"),
                    shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")),
    shiny::tags$a(href = "#workspace", class = "skip-link", "Skip to results"),
    shiny::tags$header(class = "bs-header", shiny::tags$div(class = "bs-brand",
      shiny::tags$img(src = .bs_app_logo(), alt = "", class = "bs-logo"),
      shiny::tags$span(title))),
    shiny::tags$div(class = "bs-intro", shiny::tags$h1("Slide browser")),
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 3,
        shiny::tags$details(class = "bs-filters", open = "open", shiny::tags$summary("Data & filters"),
        shiny::fileInput("upload", "Import slide results", accept = c(".csv", ".json")),
        shiny::helpText("CSV slide table or JSON result, up to 20 MiB."),
        shiny::actionButton("demo", "Load demonstration", class = "btn-default"),
        shiny::tags$hr(), shiny::selectInput("organ", group_label, choices = c("All tissues" = "")),
        shiny::selectInput("qc", "Quality stratum", choices = c("All strata" = "")),
        shiny::textInput("search", "Slide ID contains", placeholder = "e.g. S001"),
        shiny::downloadButton("download", "Export filtered CSV"),
        shiny::tags$p(class = "bs-caption", "Filters change the view, never the source measurements."))),
      shiny::mainPanel(width = 9, id = "workspace", role = "main", tabindex = "-1",
        shiny::tags$div(role = "status", `aria-live` = "polite", shiny::textOutput("status")),
        shiny::conditionalPanel("input.tab !== 'viewer'",
          shiny::uiOutput("metrics"),
          shiny::textOutput("completeness", container = function(...) shiny::tags$p(class = "bs-caption bs-completeness", ...))),
        shiny::tabsetPanel(id = "tab", selected = if (has_images) "viewer" else "Overview",
          .bs_viewer_ui(),
          shiny::tabPanel("Overview", shiny::tags$h2("Signal and candidate density"),
            shiny::plotOutput("scatter", height = "360px"),
            shiny::tags$p(class = "bs-caption", "Each point is one slide. Candidate events are not validated cell counts. Missing or zero denominators are omitted.")),
          shiny::tabPanel("Slides", shiny::tags$h2("Recorded slide measurements"),
            shiny::tags$div(class = "bs-table", shiny::tableOutput("slides"))),
          shiny::tabPanel("Compare", shiny::tags$h2("Recorded run comparison"),
            shiny::tags$p("Comparison requires a comparison table supplied with the result. No run is inferred or regenerated."),
            shiny::tags$div(class = "bs-table", shiny::tableOutput("comparison"))),
          shiny::tabPanel("Methods & provenance", shiny::tags$h2("How to read these results"),
            shiny::tags$p("Red-positive fraction = 100 \u00d7 pooled red area / pooled tissue area. Candidate density = pooled candidate spots / pooled tissue area. Slide percentages are not averaged."),
            shiny::tags$p("Missing measurements propagate to pooled summaries. Unknown tissue support produces a missing rate. Low signal does not establish a negative control. QC labels are recorded decisions, not automatic diagnoses."),
            shiny::tags$h3("Result provenance"), shiny::verbatimTextOutput("provenance")))))
  )
}

.bs_app_server <- function(initial, images = data.frame(), slide_python = "",
                           result_path = NULL, group_by = "organ") {
  function(input, output, session) {
    current <- shiny::reactiveVal(initial)
    image_manifest <- shiny::reactiveVal(images)
    watched_path <- shiny::reactiveVal(result_path)
    demo_directories <- character()
    session$onSessionEnded(function() unlink(demo_directories, recursive = TRUE))
    error <- shiny::reactiveVal(NULL)
    source_label <- function(x, fallback) {
      if (is.list(x$provenance) && identical(x$provenance$source, "Deterministic synthetic demonstration"))
        "DEMONSTRATION \u2014 synthetic measurements" else fallback
    }
    mode <- shiny::reactiveVal(if (is.null(initial)) "No results loaded" else source_label(initial, "Provided results"))
    shiny::observeEvent(input$demo, {
      demo <- bs_viewer_example()
      demo_directories <<- c(demo_directories, demo$directory)
      if (!group_by %in% names(demo$results$slides)) demo$results$slides[[group_by]] <- demo$results$slides$organ
      image_manifest(.bs_image_manifest(demo$images, demo$results$slides$physical_slide_id))
      watched_path(NULL)
      shiny::updateTabsetPanel(session, "tab", selected = "viewer")
      shiny::updateTextInput(session, "search", value = "")
      shiny::updateSelectInput(session, "organ", selected = "")
      shiny::updateSelectInput(session, "qc", selected = "")
      current(.bs_app_results(demo$results)); error(NULL); mode("DEMONSTRATION \u2014 synthetic measurements")
    })
    shiny::observeEvent(input$upload, {
      shiny::req(input$upload)
      parsed <- tryCatch(.bs_app_read(input$upload$datapath[[1L]], input$upload$name[[1L]]), error = identity)
      if (!inherits(parsed, "error") && !group_by %in% names(parsed$slides))
        parsed <- simpleError(paste("Missing grouping column:", group_by))
      if (inherits(parsed, "error")) {
        error(conditionMessage(parsed))
      } else {
        image_manifest(images); watched_path(NULL)
        shiny::updateTabsetPanel(session, "tab", selected = if (nrow(images)) "viewer" else "Overview")
        shiny::updateTextInput(session, "search", value = "")
        shiny::updateSelectInput(session, "organ", selected = "")
        shiny::updateSelectInput(session, "qc", selected = "")
        current(parsed); error(NULL); mode(source_label(parsed, paste("Imported", basename(input$upload$name[[1L]]))))
      }
    })
    shiny::observeEvent(current(), {
      d <- current()$slides
      selected_organ <- shiny::isolate(input$organ)
      if (is.null(selected_organ) || !selected_organ %in% as.character(d[[group_by]])) selected_organ <- ""
      selected_qc <- shiny::isolate(input$qc)
      if (is.null(selected_qc) || !selected_qc %in% d$qc_stratum) selected_qc <- ""
      shiny::updateSelectInput(session, "organ", choices = c("All groups" = "", stats::setNames(sort(unique(as.character(d[[group_by]]))), sort(unique(as.character(d[[group_by]])))) ), selected = selected_organ)
      shiny::updateSelectInput(session, "qc", choices = c("All strata" = "", stats::setNames(sort(unique(d$qc_stratum)), sort(unique(d$qc_stratum)))), selected = selected_qc)
    }, ignoreNULL = TRUE)
    filtered <- shiny::reactive({
      shiny::req(current())
      d <- current()$slides
      if (!is.null(input$organ) && nzchar(input$organ)) d <- d[!is.na(d[[group_by]]) & as.character(d[[group_by]]) == input$organ, , drop = FALSE]
      if (!is.null(input$qc) && nzchar(input$qc)) d <- d[d$qc_stratum == input$qc, , drop = FALSE]
      if (!is.null(input$search) && nzchar(input$search)) d <- d[grepl(input$search, d$slide_id, fixed = TRUE), , drop = FALSE]
      d
    })
    viewer <- .bs_viewer_server(input, output, session, current, filtered, image_manifest, slide_python, watched_path)
    output$status <- shiny::renderText({
      if (!is.null(error())) paste("Import failed:", error(), if (is.null(current())) "No results loaded." else "Previous results retained.") else mode()
    })
    output$metrics <- shiny::renderUI({
      if (is.null(current())) return(shiny::tags$div(class = "bs-empty", "Import your result table or explicitly load a demonstration to begin."))
      m <- .bs_app_metrics(filtered())
      labels <- c("Slides in view", "Tissue area \u00b7 mm\u00b2", "Pooled red area \u00b7 %", "Candidates \u00b7 per mm\u00b2")
      shiny::tags$div(class = "bs-metrics", lapply(seq_along(m), function(i) {
        v <- if (is.na(m[i])) "Not available" else format(round(m[i], if (i == 1L) 0 else 3), big.mark = ",", trim = TRUE)
        shiny::tags$div(class = "bs-metric", shiny::tags$span(labels[i]), shiny::tags$strong(v))
      }))
    })
    output$completeness <- shiny::renderText({
      shiny::req(current())
      d <- filtered()
      complete <- is.finite(d$tissue_area_mm2) & d$tissue_area_mm2 > 0 &
        is.finite(d$red_area_mm2) & is.finite(d$candidate_spots)
      paste0(sum(complete), " of ", nrow(d), " slides have complete measurements and positive tissue support for plotting. ",
        "Missing measurements remain unknown in pooled summaries.")
    })
    output$slides <- shiny::renderTable({ .bs_flat_table(filtered()) }, striped = TRUE, bordered = FALSE, spacing = "s", na = "Missing")
    output$scatter <- shiny::renderPlot({
      shiny::validate(shiny::need(!is.null(current()), "Load results to display measured slides."))
      d <- filtered()
      keep <- is.finite(d$tissue_area_mm2) & d$tissue_area_mm2 > 0 & is.finite(d$red_area_mm2) & is.finite(d$candidate_spots)
      d <- d[keep, , drop = FALSE]
      shiny::validate(shiny::need(nrow(d) > 0, "No slides with complete measurements and positive tissue area match these filters."))
      groups <- as.character(d[[group_by]])
      groups[is.na(groups) | !nzchar(groups)] <- "Unspecified"
      organs <- sort(unique(groups)); colors <- grDevices::hcl.colors(max(3L, length(organs)), "Dark 3")
      graphics::plot(100 * (d$red_area_mm2 / d$tissue_area_mm2), d$candidate_spots / d$tissue_area_mm2,
        xlab = "Red-positive tissue (%)", ylab = "Candidate spots / mm\u00b2",
        pch = rep(c(16, 17, 15, 18), length.out = length(organs))[match(groups, organs)],
        col = colors[match(groups, organs)], bty = "l", cex = 1.3)
      graphics::legend("topleft", legend = organs, pch = rep(c(16, 17, 15, 18), length.out = length(organs)), col = colors[seq_along(organs)], bty = "n")
    }, alt = "Scatter plot of red-positive tissue percentage against candidate spot density, with tissue indicated by color and symbol. Exact measurements are available in the Slides tab.")
    output$comparison <- shiny::renderTable({
      shiny::req(current())
      d <- current()$comparison
      shiny::validate(shiny::need(is.data.frame(d), "No recorded comparison supplied."))
      id <- if ("physical_slide_id" %in% names(d)) "physical_slide_id" else "slide_id"
      shiny::validate(shiny::need(id %in% names(d), "Comparison needs a physical slide ID column."))
      d <- d[d[[id]] %in% filtered()$slide_id, , drop = FALSE]
      d
    }, striped = TRUE, na = "Missing")
    output$provenance <- shiny::renderText({
      shiny::req(current())
      p <- current()$provenance
      if (is.null(p) || !length(p)) "No provenance supplied. Source and training claims cannot be verified from this table." else paste(utils::capture.output(utils::str(p, max.level = 3L)), collapse = "\n")
    })
    output$download <- shiny::downloadHandler(filename = function() "bloodspottR-filtered-slides.csv",
      content = function(file) .bs_app_csv(filtered(), file), contentType = "text/csv")
  }
}

.bs_app_logo <- function() {
  path <- system.file("figures", "bloodspottR_icon_noname.svg", package = "bloodspottR")
  if (!nzchar(path)) return("")
  paste0("data:image/svg+xml;base64,", jsonlite::base64_enc(readBin(path, "raw", file.info(path)$size)))
}

.bs_app_start <- function() {
  previous <- getOption("shiny.maxRequestSize")
  limit <- max(20 * 1024^2, if (is.numeric(previous)) previous else 0)
  options(shiny.maxRequestSize = limit)
  shiny::onStop(function() {
    if (identical(getOption("shiny.maxRequestSize"), limit))
      options(shiny.maxRequestSize = previous)
  })
}
