# cli's condition helpers need rlang at run time; cli itself only suggests it.
#' @importFrom rlang abort
NULL

# Preserve plain messages while exposing a catchable package condition.
.bs_abort <- function(..., call. = FALSE) {
  message <- paste0(..., collapse = "")
  cli::cli_abort("{message}", class = "bloodspottr_error", call = NULL)
}

# match.arg() semantics (first choice by default, partial matching) with a
# catchable package condition instead of a base error.
.bs_match_arg <- function(arg, choices, name) {
  tryCatch(match.arg(arg, choices), error = function(e)
    .bs_abort(name, " should be one of ", paste0('"', choices, '"', collapse = ", ")))
}
