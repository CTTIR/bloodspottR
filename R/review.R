#' Freeze a bounded image-review queue
#'
#' A client-neutral queue for saved QuPath or other annotation exports. This
#' function never launches or modifies a viewer. Image paths are explicit files;
#' each item must have a unique ID and parent-image ID. Queue sampling preserves
#' the caller's random-number state.
#' @param images Data frame with item_id, parent_id and path.
#' @param budget Maximum number of images to review.
#' @param seed Nonnegative integer sampling seed.
#' @param out New queue directory.
#' @return A bs_review_plan object referring to the durable queue.
#' @export
bs_review_plan <- function(images, budget = 50L, seed = 1L, out) {
  required <- c("item_id", "parent_id", "path")
  if (!is.data.frame(images) || anyDuplicated(names(images)) || !all(required %in% names(images)) || !nrow(images))
    stop("images needs item_id, parent_id and path rows", call. = FALSE)
  for (name in required) {
    images[[name]] <- as.character(images[[name]])
    if (anyNA(images[[name]]) || any(!nzchar(trimws(images[[name]]))))
      stop("Image identifiers and paths cannot be missing", call. = FALSE)
  }
  if (anyDuplicated(images$item_id)) stop("item_id must be unique", call. = FALSE)
  for (value in list(budget, seed))
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
        value < 0 || value != floor(value) || value > .Machine$integer.max)
      stop("budget and seed must be nonnegative integers", call. = FALSE)
  if (budget < 1) stop("budget must be positive", call. = FALSE)
  if (any(!file.exists(images$path)) || any(dir.exists(images$path)))
    stop("Image paths must be existing files", call. = FALSE)
  if (!is.character(out) || length(out) != 1L || is.na(out) || !nzchar(out) ||
      file.exists(out) || !dir.exists(dirname(out))) stop("out must be a new directory path", call. = FALSE)
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv) else
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
  set.seed(seed)
  n <- min(nrow(images), as.integer(budget))
  selected <- images[sort(sample.int(nrow(images), n)), required, drop = FALSE]
  selected$path <- vapply(selected$path, normalizePath, "", winslash = "/", mustWork = TRUE)
  selected$sha256 <- vapply(selected$path, digest::digest, "", file = TRUE, algo = "sha256")
  selected$selection <- "seeded sample from supplied image pool"
  stage <- tempfile(".bloodspottr-review-", tmpdir = dirname(out)); dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  jsonlite::write_json(list(schema_version = 1L, seed = seed, budget = budget, items = selected),
                       file.path(stage, "plan.json"), pretty = TRUE, auto_unbox = TRUE, dataframe = "rows")
  dir.create(file.path(stage, "receipts"))
  if (!file.rename(stage, out)) stop("Could not commit review plan", call. = FALSE)
  structure(list(path = normalizePath(out, winslash = "/"), items = selected), class = "bs_review_plan")
}

#' Confirm an explicitly reviewed, saved annotation export
#'
#' Saves a receipt only after reading and verifying the source image and the
#' exported CSV. It does not click Save in QuPath or infer completeness from an
#' empty file. A header-only CSV is accepted only with explicit scope assertions.
#' Parent coordinates must already be supplied by the review client's adapter.
#' @param plan A bs_review_plan object.
#' @param item_id The next incomplete review item.
#' @param annotations Saved CSV with class, x and y columns in parent coordinates.
#' @param assertions Named list of ery, nuclei and vessel review scopes. Each is
#'   unreviewed, partial, complete or absent. At least one must be reviewed.
#' @return A durable receipt, invisibly. Repeating an identical confirmation is idempotent.
#' @export
bs_review_confirm <- function(plan, item_id, annotations, assertions) {
  if (!inherits(plan, "bs_review_plan") || !is.list(plan) || !is.character(plan$path) ||
      length(plan$path) != 1L || is.na(plan$path) || !dir.exists(plan$path))
    stop("Expected an existing review plan", call. = FALSE)
  path <- plan$path
  lock <- file.path(path, ".writer-lock")
  if (!dir.create(lock, showWarnings = FALSE)) stop("Review has another writer", call. = FALSE)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  stored <- jsonlite::read_json(file.path(path, "plan.json"), simplifyVector = TRUE)
  if (!isTRUE(stored$schema_version == 1L) || !is.data.frame(stored$items))
    stop("Invalid review manifest", call. = FALSE)
  items <- stored$items
  required <- c("item_id", "parent_id", "path", "sha256")
  if (!all(required %in% names(items)) || !nrow(items) || anyDuplicated(items$item_id) ||
      any(!vapply(items[required], function(x) is.character(x) && !anyNA(x) && all(nzchar(x)), logical(1))) ||
      !dir.exists(file.path(path, "receipts"))) stop("Invalid review manifest", call. = FALSE)
  if (!is.character(item_id) || length(item_id) != 1L || is.na(item_id) || !item_id %in% items$item_id)
    stop("Unknown review item", call. = FALSE)
  allowed <- c("unreviewed", "partial", "complete", "absent")
  if (!is.list(assertions) || !setequal(names(assertions), c("ery", "nuclei", "vessel")) ||
      length(assertions) != 3L || any(!vapply(assertions, function(x)
        is.character(x) && length(x) == 1L && !is.na(x) && x %in% allowed, logical(1))) ||
      all(unlist(assertions) == "unreviewed")) stop("Explicit class-specific review assertions required", call. = FALSE)
  assertions <- assertions[c("ery", "nuclei", "vessel")]
  if (!is.character(annotations) || length(annotations) != 1L || is.na(annotations) ||
      !file.exists(annotations) || dir.exists(annotations)) stop("Saved annotation CSV required", call. = FALSE)
  before <- digest::digest(file = annotations, algo = "sha256")
  points <- utils::read.csv(annotations, stringsAsFactors = FALSE, check.names = FALSE)
  classes <- c("Spot_center", "Missed_spot", "False_positive", "Review_uncertain", "Missed_area", "False_positive_area", "Cell_nucleus", "Vessel_structure")
  if (anyDuplicated(names(points)) || !all(c("class", "x", "y") %in% names(points)) || anyNA(points$class) || any(!points$class %in% classes) ||
      !is.numeric(points$x) && nrow(points) > 0L || !is.numeric(points$y) && nrow(points) > 0L)
    stop("Invalid annotation columns or classes", call. = FALSE)
  if (nrow(points) && any(!is.finite(points$x) | !is.finite(points$y) | points$x < 0 | points$y < 0))
    stop("Annotation coordinates must be finite and nonnegative", call. = FALSE)
  targets <- list(ery = c("Spot_center", "Missed_spot", "Missed_area", "Review_uncertain"), nuclei = "Cell_nucleus", vessel = "Vessel_structure")
  for (target in names(targets)) {
    associated <- if (target == "ery") c(targets[[target]], "False_positive", "False_positive_area") else targets[[target]]
    if (assertions[[target]] == "unreviewed" && any(points$class %in% associated))
      stop("Unreviewed assertion conflicts with supplied points", call. = FALSE)
    if (assertions[[target]] == "absent" && any(points$class %in% targets[[target]]))
      stop("Absence assertion conflicts with positive or uncertain points", call. = FALSE)
  }
  index <- match(item_id, items$item_id)
  if (!file.exists(items$path[index]) || digest::digest(file = items$path[index], algo = "sha256") != items$sha256[index])
    stop("Review image changed", call. = FALSE)
  receipt_path <- file.path(path, "receipts", sprintf("%06d.json", index))
  archive <- file.path(path, "receipts", sprintf("%06d.csv", index))
  if (file.exists(receipt_path)) {
    receipt <- jsonlite::read_json(receipt_path, simplifyVector = FALSE)
    if (!identical(receipt$status, "complete") || !identical(receipt$item_id, item_id) ||
        !identical(receipt$image_sha256, items$sha256[index]) || !file.exists(archive) ||
        !identical(digest::digest(file = archive, algo = "sha256"), before) ||
        !identical(receipt$annotation_sha256, before) || !identical(receipt$assertions, assertions))
      stop("Completed item differs; create a new review revision", call. = FALSE)
    if (!identical(digest::digest(file = annotations, algo = "sha256"), before))
      stop("Annotations changed during readback", call. = FALSE)
    return(invisible(receipt))
  }
  complete <- file.exists(file.path(path, "receipts", sprintf("%06d.json", seq_len(nrow(items)))))
  for (done in which(complete)) {
    prior <- jsonlite::read_json(file.path(path, "receipts", sprintf("%06d.json", done)))
    prior_csv <- file.path(path, "receipts", sprintf("%06d.csv", done))
    if (!identical(prior$status, "complete") || !identical(prior$item_id, items$item_id[done]) ||
        !identical(prior$image_sha256, items$sha256[done]) || !file.exists(prior_csv) ||
        dir.exists(prior_csv) || !identical(prior$annotation_sha256, digest::digest(file = prior_csv, algo = "sha256")))
      stop("Earlier review receipt or archive is invalid", call. = FALSE)
  }
  if (index != which(!complete)[1L]) stop("Confirm the next incomplete item first", call. = FALSE)
  if (file.exists(archive)) {
    if (dir.exists(archive) || !identical(digest::digest(file = archive, algo = "sha256"), before))
      stop("Uncommitted annotation archive differs; preserve it and create a new revision", call. = FALSE)
  } else if (!file.copy(annotations, archive, overwrite = FALSE)) stop("Could not archive annotations", call. = FALSE)
  if (before != digest::digest(file = archive, algo = "sha256") || before != digest::digest(file = annotations, algo = "sha256")) {
    unlink(archive); stop("Annotations changed during readback", call. = FALSE)
  }
  receipt <- list(schema_version = 1L, item_id = item_id, parent_id = items$parent_id[index],
    image_sha256 = items$sha256[index], annotation_sha256 = before, n_points = nrow(points),
    assertions = assertions, status = "complete",
    saved_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  temporary <- tempfile(".receipt-", tmpdir = file.path(path, "receipts"))
  on.exit(unlink(temporary), add = TRUE)
  jsonlite::write_json(receipt, temporary, auto_unbox = TRUE, pretty = TRUE)
  if (!file.rename(temporary, receipt_path)) stop("Could not commit receipt", call. = FALSE)
  invisible(receipt)
}
