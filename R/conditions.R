# cli's condition helpers need rlang at run time; cli itself only suggests it.
#' @importFrom rlang abort
NULL

# Preserve plain messages while exposing a catchable package condition.
.bs_abort <- function(..., call. = FALSE) {
  message <- paste0(..., collapse = "")
  cli::cli_abort("{message}", class = "bloodspottr_error", call = NULL)
}
