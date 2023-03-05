
#' @import stringr
#' @import purrr
#' @import tibble
#' @import dplyr
NULL

msg <- function(..., collapse = "", col = "steelblue3", new_line = TRUE, verbose = TRUE) {
  msg <- str_c(list(...), collapse = collapse)
  if (verbose && new_line) {
    cli::cat_line(msg, col = col)
  } else if (verbose) {
    cat(crayon::blue(msg))
  }
}
