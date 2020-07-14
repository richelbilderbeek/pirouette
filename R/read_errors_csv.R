#' Read the errors from a \code{.csv} file
#'
#' @inheritParams default_params_doc
#' @return a numeric vector of error values
#' @author Richèl J.C. Bilderbeek
#' @export
read_errors_csv <- function(errors_filename) {
  if (!file.exists(errors_filename)) {
    stop("'errors_filename' not found. Actual value: ", errors_filename)
  }
  # The expression below gives the following warning message:
  #
  # Missing column names filled in: 'X1' [1]
  #
  # Posted Issue at https://github.com/tidyverse/readr/issues/1103
  suppressWarnings(
    readr::read_csv(
      errors_filename,
      col_types = readr::cols(
        X1 = readr::col_skip(),
        x = readr::col_double()
      )
    )$x
  )
}
