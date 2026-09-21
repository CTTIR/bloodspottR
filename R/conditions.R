# Preserve plain messages while exposing a catchable package condition.
.bs_abort <- function(..., call. = FALSE) {
  message <- paste0(..., collapse = "")
  cli::cli_abort("{message}", class = "bloodspottr_error", call = NULL)
}
