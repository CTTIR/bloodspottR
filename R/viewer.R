# Only caller-supplied local paths are registered with the current Shiny session.
.bs_image_manifest <- function(images, slide_ids) {
  if (is.null(images)) return(data.frame())
  base <- getwd()
  if (is.character(images) && length(images) == 1L) {
    base <- dirname(normalizePath(images, mustWork = TRUE))
    images <- utils::read.csv(images, colClasses = "character", check.names = FALSE,
                             na.strings = "")
  }
  needed <- c("image_id", "slide_id", "path")
  if (!is.data.frame(images) || !nrow(images) || anyDuplicated(names(images)) ||
      !all(needed %in% names(images))) .bs_abort("images needs image_id, slide_id and path columns")
  for (name in needed) {
    if (!is.character(images[[name]]) || anyNA(images[[name]]) ||
        any(!nzchar(trimws(images[[name]])))) .bs_abort("Image IDs and paths must be nonempty strings")
  }
  if (anyDuplicated(images$image_id) || any(!images$slide_id %in% slide_ids))
    .bs_abort("Image IDs must be unique and slide_id must match the results")
  for (name in intersect(c("path", "annotations"), names(images))) {
    images[[name]] <- vapply(images[[name]], function(x) {
      if (is.na(x) || !nzchar(x)) return("")
      if (!grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", x)) x <- file.path(base, x)
      if (!file.exists(x) || dir.exists(x)) .bs_abort("Image or annotation file does not exist: ", x)
      normalizePath(x, winslash = "/", mustWork = TRUE)
    }, "")
  }
  if (!"label" %in% names(images)) images$label <- images$image_id
  if (!is.character(images$label) || anyNA(images$label)) .bs_abort("Image labels must be strings")
  for (name in c("scale_x", "scale_y", "offset_x", "offset_y")) {
    if (!name %in% names(images)) images[[name]] <- if (startsWith(name, "scale")) 1 else 0
    images[[name]] <- suppressWarnings(as.numeric(images[[name]]))
    if (any(!is.finite(images[[name]])) ||
        (startsWith(name, "scale") && any(images[[name]] <= 0)))
      .bs_abort("Image coordinate transforms must be finite with positive scales")
  }
  images
}

.bs_slide_python <- function(python, args) {
  if (!requireNamespace("processx", quietly = TRUE)) .bs_abort("Native slides require processx")
  if (!is.character(python) || length(python) != 1L || is.na(python) || !nzchar(python))
    .bs_abort("Set slide_python to a Python executable with openslide-python and openslide-bin installed")
  worker <- system.file("python", "slide_tiles.py", package = "bloodspottR")
  result <- processx::run(python, c(worker, args), error_on_status = FALSE,
                         timeout = 30, cleanup_tree = TRUE)
  if (result$status != 0L) .bs_abort("Native slide reader failed: ", result$stderr)
  result$stdout
}

.bs_image_info <- function(row, python, cache) {
  path <- row$path[[1L]]
  ext <- tolower(tools::file_ext(path))
  type <- if (ext %in% c("png", "jpg", "jpeg")) "image" else "native"
  info <- list(type = type, path = path, python = python, cache = cache,
               mime = if (ext == "png") "image/png" else "image/jpeg")
  if (type == "native") {
    metadata <- jsonlite::fromJSON(.bs_slide_python(python, c("info", path)))
    info <- c(info, metadata)
  }
  info
}

.bs_image_response <- function(data, req) {
  tryCatch({
    path <- data$path
    if (data$type == "native") {
      query <- shiny::parseQueryString(req$QUERY_STRING)
      keys <- c("level", "x", "y")
      if (!all(keys %in% names(query)) || any(!vapply(query[keys], function(v)
          length(v) == 1L && grepl("^[0-9]{1,9}$", v), logical(1))))
        return(shiny::httpResponse(400L, "text/plain", "Invalid tile coordinates"))
      level <- as.numeric(query$level); x <- as.numeric(query$x); y <- as.numeric(query$y)
      if (level > data$max_level) return(shiny::httpResponse(400L, "text/plain", "Invalid tile level"))
      signature <- paste(data$path, file.info(data$path)$mtime, file.info(data$path)$size, collapse = "|")
      key <- digest::digest(list(signature, level, x, y), algo = "sha256")
      path <- file.path(data$cache, paste0(key, ".png"))
      if (!file.exists(path)) {
        .bs_slide_python(data$python, c("tile", data$path, level, x, y, path))
        old <- list.files(data$cache, full.names = TRUE)
        if (length(old) > 256L) unlink(old[order(file.info(old)$mtime)][seq_len(length(old) - 256L)])
      }
    }
    shiny::httpResponse(200L, if (data$type == "native") "image/png" else data$mime,
      readBin(path, "raw", file.info(path)$size), headers = list("Cache-Control" = "private, max-age=60"))
  }, error = function(e) shiny::httpResponse(422L, "text/plain", "Slide tile could not be read"))
}

.bs_annotations <- function(row) {
  if (!"annotations" %in% names(row) || !nzchar(row$annotations[[1L]])) return(list())
  path <- row$annotations[[1L]]
  if (file.info(path)$size > 20 * 1024^2) .bs_abort("Annotation file exceeds 20 MiB; use a smaller review region or a rendered overlay")
  if (grepl("\\.csv(\\.gz)?$", path, ignore.case = TRUE)) {
    d <- utils::read.csv(path, check.names = FALSE, colClasses = "character", na.strings = "")
    if (!all(c("x", "y") %in% names(d)) || anyDuplicated(names(d)))
      .bs_abort("Annotation CSV requires unique x and y columns in image pixels")
    if (nrow(d) > 10000L) .bs_abort("At most 10000 vector annotations may be displayed")
    features <- lapply(seq_len(nrow(d)), function(i) list(type = "Feature",
      id = if ("annotation_id" %in% names(d)) d$annotation_id[i] else as.character(i),
      geometry = list(type = "Point", coordinates = suppressWarnings(as.numeric(d[i, c("x", "y")]))),
      properties = as.list(d[i, setdiff(names(d), c("x", "y")), drop = FALSE])))
  } else {
    raw <- jsonlite::read_json(path, simplifyVector = FALSE)
    features <- if (identical(raw$type, "FeatureCollection")) raw$features else
      if (identical(raw$type, "Feature")) list(raw) else .bs_abort("Annotations must be GeoJSON features or a FeatureCollection")
  }
  if (!is.list(features) || length(features) > 10000L)
    .bs_abort("At most 10000 vector annotations may be displayed; use a region or rendered overlay for dense detections")
  ids <- character(length(features))
  transform <- function(coords) {
    if (is.list(coords) && all(vapply(coords, is.list, logical(1)))) return(lapply(coords, transform))
    values <- unlist(coords, use.names = FALSE)
    if (!is.numeric(values) || length(values) != 2L || any(!is.finite(values)))
      .bs_abort("Annotation coordinates must be finite two-dimensional pixel coordinates")
    as.list(c(values[1] * row$scale_x + row$offset_x, values[2] * row$scale_y + row$offset_y))
  }
  for (i in seq_along(features)) {
    f <- features[[i]]
    if (!identical(f$type, "Feature") || !is.list(f$geometry) ||
        !f$geometry$type %in% c("Point", "MultiPoint", "LineString", "MultiLineString", "Polygon", "MultiPolygon"))
      .bs_abort("Unsupported annotation geometry; export 2D points, lines or polygons")
    f$id <- if (is.null(f$id)) as.character(i) else as.character(f$id)
    if (length(f$id) != 1L || is.na(f$id) || !nzchar(f$id)) .bs_abort("Annotation IDs must be nonempty")
    ids[i] <- f$id
    depth <- switch(f$geometry$type, Point = 0L, MultiPoint = 1L,
      LineString = 1L, MultiLineString = 2L, Polygon = 2L, MultiPolygon = 3L)
    valid_shape <- function(x, level) {
      if (level == 0L) return(length(x) == 2L && all(vapply(x, function(v)
        is.numeric(v) && length(v) == 1L && is.finite(v), logical(1))))
      is.list(x) && length(x) > 0L && all(vapply(x, valid_shape, logical(1), level - 1L))
    }
    if (!valid_shape(f$geometry$coordinates, depth)) .bs_abort("Invalid annotation geometry coordinates")
    f$geometry$coordinates <- transform(f$geometry$coordinates)
    if (is.null(f$properties)) f$properties <- list()
    if (!is.list(f$properties)) .bs_abort("Annotation properties must be an object")
    features[[i]] <- f
  }
  if (anyDuplicated(ids)) .bs_abort("Annotation IDs must be unique within an image")
  features
}

.bs_viewer_ui <- function() {
  shiny::tabPanel("Slide viewer", value = "viewer",
    shiny::tags$div(class = "bs-viewer-tools",
      shiny::actionButton("previous_slide", "Previous"),
      shiny::selectInput("selected_slide", "Slide", choices = character()),
      shiny::actionButton("next_slide", "Next"),
      shiny::selectInput("selected_image", "Image / layer", choices = character())),
    shiny::tags$div(class = "bs-viewer-tools",
      shiny::tags$button(id = "viewer-home", type = "button", "Fit image"),
      shiny::tags$button(id = "viewer-in", type = "button", "Zoom in"),
      shiny::tags$button(id = "viewer-out", type = "button", "Zoom out"),
      shiny::checkboxInput("show_annotations", "Show annotations", TRUE),
      shiny::actionButton("refresh_slide", "Refresh saved data"),
      shiny::checkboxInput("live_refresh", "Watch saved files", FALSE)),
    shiny::textOutput("viewer_status"),
    shiny::tags$div(class = "bs-inspection",
      shiny::tags$div(id = "slide-viewer", role = "region", `aria-label` = "Slide image; drag to pan and use zoom buttons", tabindex = "0"),
      shiny::tags$div(class = "bs-slide-details",
        shiny::tabsetPanel(id = "slide_detail_tab",
          shiny::tabPanel("Measurements", value = "measurements", shiny::tableOutput("selected_measurements")),
          shiny::tabPanel("Annotation", value = "annotation",
            shiny::selectInput("selected_annotation", "Annotation", choices = character()),
            shiny::tableOutput("annotation_values"),
            shiny::tags$details(shiny::tags$summary("All recorded properties"),
              shiny::verbatimTextOutput("annotation_measurements")))))))
}

.bs_viewer_server <- function(input, output, session, current, filtered, images,
                              slide_python, result_path = NULL) {
  cache <- tempfile("bloodspottr-tiles-"); dir.create(cache)
  session$onSessionEnded(function() unlink(cache, recursive = TRUE))
  info_cache <- new.env(parent = emptyenv())
  refresh <- shiny::reactiveVal(0L)
  failure <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$refresh_slide, { refresh(refresh() + 1L) })
  shiny::observe({
    if (isTRUE(input$live_refresh)) {
      shiny::invalidateLater(2000, session)
      refresh(shiny::isolate(refresh()) + 1L)
    }
  })
  shiny::observeEvent(refresh(), {
    path <- result_path()
    if (!is.null(path)) {
      updated <- tryCatch(.bs_app_results(bs_import_legacy(path)), error = identity)
      if (inherits(updated, "error")) failure(conditionMessage(updated)) else {
        if (!identical(updated, shiny::isolate(current()))) current(updated)
        failure(NULL)
      }
    }
  }, ignoreInit = TRUE)
  shiny::observeEvent(filtered(), {
    ids <- filtered()$slide_id
    selected <- shiny::isolate(input$selected_slide)
    if (is.null(selected) || !selected %in% ids) selected <- if (length(ids)) ids[1L] else ""
    shiny::updateSelectInput(session, "selected_slide", choices = ids, selected = selected)
  })
  move <- function(step) {
    ids <- filtered()$slide_id
    index <- match(input$selected_slide, ids)
    if (length(ids) && !is.na(index)) {
      next_index <- max(1L, min(length(ids), index + step))
      shiny::updateSelectInput(session, "selected_slide", selected = ids[next_index])
    }
  }
  shiny::observeEvent(input$previous_slide, move(-1L))
  shiny::observeEvent(input$next_slide, move(1L))
  available <- shiny::reactive({
    d <- images()
    if (!nrow(d) || is.null(input$selected_slide) || !input$selected_slide %in% filtered()$slide_id) return(data.frame())
    d[d$slide_id == input$selected_slide, , drop = FALSE]
  })
  shiny::observeEvent(available(), {
    d <- available()
    shiny::updateSelectInput(session, "selected_image",
      choices = if (nrow(d)) stats::setNames(d$image_id, d$label) else character(),
      selected = if (nrow(d)) d$image_id[1L] else "")
  })
  payload <- shiny::reactive({
    refresh()
    d <- available()
    if (!nrow(d) || is.null(input$selected_image) || !input$selected_image %in% d$image_id)
      return(list(empty = TRUE, status = "No image linked to this slide. Supply an images manifest when opening the app."))
    row <- d[d$image_id == input$selected_image, , drop = FALSE]
    tryCatch({
      signature <- digest::digest(list(row$path, file.info(row$path)$mtime, file.info(row$path)$size))
      if (!exists(signature, info_cache, inherits = FALSE)) {
        info <- .bs_image_info(row, slide_python, cache)
        info$url <- session$registerDataObj(paste0("slide-", signature), info, .bs_image_response)
        assign(signature, info, info_cache)
      }
      info <- get(signature, info_cache, inherits = FALSE)
      features <- .bs_annotations(row)
      url <- info$url
      list(image_id = row$image_id, type = info$type, url = url,
           width = info$width, height = info$height, max_level = info$max_level,
           features = features, empty = FALSE,
           status = paste(row$label, "\u00b7", length(features), "vector annotations"))
    }, error = function(e) list(empty = TRUE, status = conditionMessage(e)))
  })
  shiny::observeEvent(payload(), {
    p <- payload()
    session$sendCustomMessage("bloodspottr-image", p)
    labels <- vapply(p$features, function(f) {
      cls <- f$properties$classification
      label <- if (is.list(cls)) cls$name else f$properties$class
      paste(f$id, if (is.character(label) && length(label) == 1L) label else "")
    }, "")
    ids <- vapply(p$features, function(f) f$id, "")
    selected <- shiny::isolate(input$selected_annotation)
    if (is.null(selected) || !selected %in% ids) selected <- ""
    shiny::updateSelectInput(session, "selected_annotation",
      choices = c("Select an annotation" = "", stats::setNames(ids, labels)), selected = selected)
  })
  shiny::observeEvent(input$viewer_annotation, {
    hit <- input$viewer_annotation
    p <- payload()
    if (identical(hit$image_id, p$image_id) && hit$id %in% vapply(p$features, function(f) f$id, ""))
      {
        shiny::updateSelectInput(session, "selected_annotation", selected = hit$id)
        shiny::updateTabsetPanel(session, "slide_detail_tab", selected = "annotation")
      }
  })
  shiny::observeEvent(input$selected_annotation, {
    session$sendCustomMessage("bloodspottr-select", list(id = input$selected_annotation))
  })
  shiny::observeEvent(input$show_annotations, {
    session$sendCustomMessage("bloodspottr-annotations", isTRUE(input$show_annotations))
  })
  output$viewer_status <- shiny::renderText({
    if (!is.null(failure())) paste("Saved-data refresh failed; previous measurements retained:", failure()) else
      if (!is.null(input$viewer_load_error)) input$viewer_load_error else payload()$status
  })
  output$selected_measurements <- shiny::renderTable({
    d <- filtered()
    d <- d[d$slide_id == input$selected_slide, , drop = FALSE]
    shiny::req(nrow(d) == 1L)
    d <- .bs_flat_table(d)
    priority <- intersect(c("report_id", "slide_id", "organ", "tissue_area_mm2", "red_area_mm2", "red_percent", "candidate_spots", "operational_events"), names(d))
    d <- d[c(priority, setdiff(names(d), priority))]
    data.frame(Measurement = gsub("_", " ", names(d), fixed = TRUE), Value = vapply(d, function(v)
      if (is.na(v[1])) "Missing" else as.character(v[1]), ""))
  }, striped = TRUE)
  output$annotation_values <- shiny::renderTable({
    f <- payload()$features
    selected <- which(vapply(f, function(x) identical(x$id, input$selected_annotation), logical(1)))
    shiny::validate(shiny::need(length(selected), "Select an annotation to inspect its measurements."))
    .bs_annotation_values(f[[selected[1L]]]$properties)
  }, striped = TRUE)
  output$annotation_measurements <- shiny::renderText({
    f <- payload()$features
    selected <- which(vapply(f, function(x) identical(x$id, input$selected_annotation), logical(1)))
    if (!length(selected)) return("Click an annotation in the image or choose it above.")
    as.character(jsonlite::toJSON(f[[selected[1L]]]$properties, auto_unbox = TRUE,
      pretty = TRUE, null = "null", na = "null", digits = NA))
  })
  invisible(list(payload = payload, available = available))
}

.bs_annotation_values <- function(properties) {
  rows <- list()
  add <- function(name, value) {
    text <- if (is.null(value)) "Missing" else if (is.atomic(value) && length(value) == 1L)
      if (is.na(value)) "Missing" else as.character(value) else
        as.character(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", na = "null"))
    rows[[length(rows) + 1L]] <<- data.frame(Measurement = name, Value = text)
  }
  for (name in names(properties)) {
    value <- properties[[name]]
    if (name == "classification" && is.list(value)) add("Class", value$name) else
      if (name == "measurements" && is.list(value)) {
        if (!is.null(names(value))) {
          for (key in names(value)) add(key, value[[key]])
        } else for (measurement in value) {
          if (is.list(measurement) && is.character(measurement$name) && length(measurement$name) == 1L)
            add(measurement$name, measurement$value)
          else add("Measurement", measurement)
        }
      } else add(gsub("_", " ", name, fixed = TRUE), value)
  }
  if (!length(rows)) return(data.frame(Measurement = "Properties", Value = "None supplied"))
  do.call(rbind, rows)
}
