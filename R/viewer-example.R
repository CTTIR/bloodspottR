#' Create a synthetic annotated-slide viewer example
#'
#' Writes eight small PNG images and GeoJSON annotation files to a temporary
#' directory. Tissue labels span liver, spleen, lung and kidney; no study data is
#' used. Pass the returned results and images to `bs_app()`. Delete `directory`
#' when finished with the example.
#' @return A list with results, images manifest and temporary directory.
#' @export
#' @examples
#' demo <- bs_viewer_example()
#' if (interactive()) bs_app(demo$results, images = demo$images)
#' unlink(demo$directory, recursive = TRUE)
bs_viewer_example <- function() {
  root <- tempfile("bloodspottr-viewer-"); dir.create(root)
  results <- bs_example()
  results$slides$tissue_area_mm2[4] <- 1
  results$slides$red_area_mm2 <- results$slides$tissue_area_mm2 * .01
  results$slides$red_area_mm2[8] <- NA_real_
  results$slides$candidate_spots <- rep(12, 8)
  results$slides$unresolved_bulk_events <- rep(0, 8)
  results$slides$operational_events <- rep(12, 8)
  results$slides$organ <- rep(c("Liver", "Spleen", "Lung", "Kidney"), each = 2L)
  images <- data.frame(image_id = paste0("image-", seq_len(8)),
    slide_id = results$slides$physical_slide_id,
    path = file.path(root, paste0(seq_len(8), ".png")),
    annotations = file.path(root, paste0(seq_len(8), ".geojson")),
    label = paste(results$slides$organ, "preview"))
  for (i in seq_len(8)) {
    grDevices::png(images$path[i], width = 720, height = 480)
    tryCatch({
      graphics::par(mar = rep(0, 4), xaxs = "i", yaxs = "i")
      graphics::plot.new(); graphics::plot.window(c(0, 720), c(480, 0))
      graphics::rect(0, 0, 720, 480, col = "#f5eee7", border = NA)
      graphics::polygon(c(30, 120, 310, 650, 690, 500, 180),
        c(220, 70, 45, 120, 310, 420, 400), col = "#e8c8c5", border = "#d0aaa9")
      for (j in seq_len(12)) {
        x <- 85 + (j * 47 + i * 13) %% 530
        y <- 90 + (j * 61 + i * 17) %% 280
        graphics::symbols(x, y, circles = 12 + j %% 5, inches = FALSE,
          add = TRUE, bg = "#992333", fg = "#7b192c")
      }
      graphics::text(360, 452, paste("SYNTHETIC", results$slides$organ[i], results$slides$physical_slide_id[i]),
        col = "#563c48", cex = 1.1)
    }, finally = grDevices::dev.off())
    features <- lapply(seq_len(12), function(j) list(type = "Feature", id = paste0("candidate-", j),
      geometry = list(type = "Point", coordinates = c(85 + (j * 47 + i * 13) %% 530,
                                                     90 + (j * 61 + i * 17) %% 280)),
      properties = list(classification = list(name = "Candidate"),
        measurements = list(list(name = "Illustrative diameter", value = 24 + 2 * (j %% 5))),
        units = "preview pixels", scope = "Synthetic illustration; not biological evidence")))
    jsonlite::write_json(list(type = "FeatureCollection", features = features), images$annotations[i], auto_unbox = TRUE)
  }
  list(results = results, images = images, directory = root)
}
