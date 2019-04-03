#' Converts true tree filenames to twin tree filenames
#' @inheritParams default_params_doc
#' @return twin tree filename
#' @export
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' filename <- "beast2_output.xml.state"
#' created <- to_twin_filename(filename)
#' expected <- "beast2_output_twin.xml.state"
#' expect_equal(expected, created)
to_twin_filename <- function(
  filename
) {
  stringr::str_replace(string = filename, pattern = "\\.", "_twin.")
}
