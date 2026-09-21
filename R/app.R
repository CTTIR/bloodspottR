#' Explore slide results in a local Shiny application
#'
#' Presents imported measurements without altering scientific values. CSV uploads
#' contain a slide table; JSON uploads contain a result object with a `slides`
#' table. Uploaded files are parsed as data, never evaluated as R code.
#' @param results A result list with a `slides` data frame, or `NULL`.
#' @param launch Launch the application; `FALSE` returns a Shiny application.
#' @return A Shiny application object, invisibly when launched.
#' @export
bs_app <- function(results = NULL, launch = interactive()) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Install the optional 'shiny' package to use bs_app().", call. = FALSE)
  }
  if (!is.logical(launch) || length(launch) != 1L || is.na(launch)) {
    stop("launch must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(results)) results <- .bs_app_results(results)
  app <- shiny::shinyApp(.bs_app_ui(), .bs_app_server(results))
  if (launch) shiny::runApp(app) else app
}

.bs_app_results <- function(x) {
  if (!is.list(x) || !is.data.frame(x$slides)) {
    stop("Results must contain a slides data frame.", call. = FALSE)
  }
  d <- x$slides
  if (anyDuplicated(names(d))) stop("Slide columns must have unique names.", call. = FALSE)
  if (!"slide_id" %in% names(d) && "physical_slide_id" %in% names(d)) d$slide_id <- as.character(d$physical_slide_id)
  if (!"qc_stratum" %in% names(d) && "stratum" %in% names(d)) d$qc_stratum <- d$stratum
  required <- c("slide_id", "tissue_area_mm2", "red_area_mm2", "candidate_spots")
  missing <- setdiff(required, names(d))
  if (length(missing)) stop("Missing slide columns: ", paste(missing, collapse = ", "), call. = FALSE)
  if (anyNA(d$slide_id) || any(!nzchar(as.character(d$slide_id))) || anyDuplicated(d$slide_id)) {
    stop("Slide IDs must be nonmissing and unique.", call. = FALSE)
  }
  numeric_fields <- c(required[-1L], intersect(c("unresolved_bulk_events", "operational_events"), names(d)))
  for (name in numeric_fields) {
    if (is.logical(d[[name]]) && all(is.na(d[[name]]))) d[[name]] <- as.numeric(d[[name]])
    v <- d[[name]]
    if (!is.numeric(v) || any(is.nan(v)) || any(!is.na(v) & (!is.finite(v) | v < 0))) {
      stop(name, " must contain nonnegative finite numbers or NA.", call. = FALSE)
    }
  }
  if (any(d$red_area_mm2 > d$tissue_area_mm2, na.rm = TRUE)) {
    stop("Red area cannot exceed tissue area.", call. = FALSE)
  }
  for (name in intersect(c("candidate_spots", "unresolved_bulk_events", "operational_events"), names(d))) {
    if (any(d[[name]] != floor(d[[name]]) | d[[name]] > 2^53, na.rm = TRUE)) {
      stop("Event counts must be exactly representable whole counts.", call. = FALSE)
    }
  }
  if (all(c("unresolved_bulk_events", "operational_events") %in% names(d))) {
    if (any(d$candidate_spots > 2^53 - d$unresolved_bulk_events, na.rm = TRUE))
      stop("Combined event counts exceed exact double precision.", call. = FALSE)
    if (any(d$operational_events != d$candidate_spots + d$unresolved_bulk_events, na.rm = TRUE))
      stop("Operational events must equal spots plus bulk events.", call. = FALSE)
  }
  any_count <- d$candidate_spots > 0
  if ("operational_events" %in% names(d)) any_count <- any_count | d$operational_events > 0
  if (any(d$tissue_area_mm2 == 0 & any_count, na.rm = TRUE)) {
    stop("Positive counts require positive tissue support.", call. = FALSE)
  }
  d$slide_id <- as.character(d$slide_id)
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
  if (!ext %in% c("csv", "json")) stop("Choose a CSV or JSON data file.", call. = FALSE)
  resolved <- normalizePath(path, winslash = "/", mustWork = FALSE)
  upload_root <- paste0(normalizePath(tempdir(), winslash = "/", mustWork = TRUE), "/")
  if (!startsWith(resolved, upload_root)) stop("Upload must be a session temporary file.", call. = FALSE)
  size <- file.info(path)$size
  if (is.na(size) || size > 20 * 1024^2) stop("Upload must be readable and at most 20 MiB.", call. = FALSE)
  if (ext == "csv") {
    d <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                         colClasses = "character")
    for (n in intersect(c("tissue_area_mm2", "red_area_mm2", "candidate_spots",
                          "unresolved_bulk_events", "operational_events", "red_percent",
                          "spots_per_mm2", "events_per_mm2"), names(d))) {
      raw <- d[[n]]
      d[[n]] <- suppressWarnings(as.numeric(raw))
      if (any(!is.na(raw) & nzchar(raw) & is.na(d[[n]]))) stop("Invalid numeric column: ", n, call. = FALSE)
    }
    x <- list(slides = d, provenance = list(source = basename(name)))
  } else {
    if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite to read JSON.", call. = FALSE)
    x <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
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
      if (v > remaining) stop("Pooled counts exceed exact double precision.", call. = FALSE)
      remaining <- remaining - v
    }
  }
  spots <- sum(d$candidate_spots)
  if (any(is.infinite(c(tissue, red)))) stop("Pooled measurements exceed finite numeric range.", call. = FALSE)
  values <- c(slides = nrow(d), tissue = tissue,
    red_percent = if (is.finite(tissue) && tissue > 0) 100 * (red / tissue) else NA_real_,
    spots_per_mm2 = if (is.finite(tissue) && tissue > 0) spots / tissue else NA_real_)
  if (any(is.infinite(values))) stop("Pooled rates exceed finite numeric range.", call. = FALSE)
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

.bs_app_ui <- function() {
  css_path <- system.file("app", "style.css", package = "bloodspottR")
  css <- if (nzchar(css_path)) paste(readLines(css_path, warn = FALSE), collapse = "\n") else ""
  shiny::fluidPage(
    shiny::tags$head(shiny::tags$style(shiny::HTML(css)),
                    shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")),
    shiny::tags$a(href = "#workspace", class = "skip-link", "Skip to results"),
    shiny::tags$header(class = "bs-header", shiny::tags$div(class = "bs-brand", "bloodspottR"),
      shiny::tags$p("Calibrated histology \u00b7 transparent evidence")),
    shiny::tags$div(class = "bs-intro", shiny::tags$h1("Slide evidence workspace"),
      shiny::tags$p("Explore physical-slide measurements, review quality strata and compare recorded runs.")),
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 3,
        shiny::tags$h2("Data & filters"),
        shiny::fileInput("upload", "Import slide results", accept = c(".csv", ".json")),
        shiny::helpText("CSV slide table or JSON result, up to 20 MiB. Local session only."),
        shiny::actionButton("demo", "Load demonstration", class = "btn-default"),
        shiny::tags$hr(), shiny::selectInput("organ", "Tissue", choices = "All"),
        shiny::selectInput("qc", "Quality stratum", choices = "All"),
        shiny::textInput("search", "Slide ID contains", placeholder = "e.g. S001"),
        shiny::downloadButton("download", "Export filtered CSV"),
        shiny::tags$p(class = "bs-caption", "Filters change the view, never the source measurements.")),
      shiny::mainPanel(width = 9, id = "workspace",
        shiny::tags$div(role = "status", `aria-live` = "polite", shiny::textOutput("status")),
        shiny::uiOutput("metrics"),
        shiny::tabsetPanel(id = "tab",
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

.bs_app_server <- function(initial) {
  function(input, output, session) {
    current <- shiny::reactiveVal(initial)
    error <- shiny::reactiveVal(NULL)
    mode <- shiny::reactiveVal(if (is.null(initial)) "No results loaded" else "Provided results")
    shiny::observeEvent(input$demo, {
      current(.bs_app_results(bs_example())); error(NULL); mode("DEMONSTRATION \u2014 synthetic measurements")
    })
    shiny::observeEvent(input$upload, {
      shiny::req(input$upload)
      parsed <- tryCatch(.bs_app_read(input$upload$datapath[[1L]], input$upload$name[[1L]]), error = identity)
      if (inherits(parsed, "error")) {
        error(conditionMessage(parsed))
      } else {
        current(parsed); error(NULL); mode(paste("Imported", basename(input$upload$name[[1L]])))
      }
    })
    shiny::observeEvent(current(), {
      d <- current()$slides
      shiny::updateSelectInput(session, "organ", choices = c("All", sort(unique(d$organ))), selected = "All")
      shiny::updateSelectInput(session, "qc", choices = c("All", sort(unique(d$qc_stratum))), selected = "All")
    }, ignoreNULL = TRUE)
    filtered <- shiny::reactive({
      shiny::req(current())
      d <- current()$slides
      if (!is.null(input$organ) && input$organ != "All") d <- d[d$organ == input$organ, , drop = FALSE]
      if (!is.null(input$qc) && input$qc != "All") d <- d[d$qc_stratum == input$qc, , drop = FALSE]
      if (!is.null(input$search) && nzchar(input$search)) d <- d[grepl(input$search, d$slide_id, fixed = TRUE), , drop = FALSE]
      d
    })
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
    output$slides <- shiny::renderTable({ filtered() }, striped = TRUE, bordered = FALSE, spacing = "s", na = "Missing")
    output$scatter <- shiny::renderPlot({
      shiny::validate(shiny::need(!is.null(current()), "Load results to display measured slides."))
      d <- filtered()
      keep <- is.finite(d$tissue_area_mm2) & d$tissue_area_mm2 > 0 & is.finite(d$red_area_mm2) & is.finite(d$candidate_spots)
      d <- d[keep, , drop = FALSE]
      shiny::validate(shiny::need(nrow(d) > 0, "No slides with complete measurements and positive tissue area match these filters."))
      organs <- sort(unique(d$organ)); colors <- grDevices::hcl.colors(max(3L, length(organs)), "Dark 3")
      graphics::plot(100 * (d$red_area_mm2 / d$tissue_area_mm2), d$candidate_spots / d$tissue_area_mm2,
        xlab = "Red-positive tissue (%)", ylab = "Candidate spots / mm\u00b2", pch = 19,
        col = colors[match(d$organ, organs)], bty = "l", cex = 1.3)
      graphics::legend("topleft", legend = organs, pch = 19, col = colors[seq_along(organs)], bty = "n")
    }, alt = "Scatter plot of red-positive tissue percentage against candidate spot density, colored by tissue. Exact measurements are available in the Slides tab.")
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
