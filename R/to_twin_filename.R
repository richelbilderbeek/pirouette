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
  # Get the basename with extension
  base_filename <- basename(filename)

  # Replace the first dot with '_twin.'
  twin_basename <- stringr::str_replace(
    string = base_filename,
    pattern = "\\.", "_twin."
  )

  # Complete the path
  twin_path <- file.path(
    dirname(filename),
    twin_basename
  )

  # Remove the './' at the beginning if present
  stringr::str_replace(
    string = twin_path,
    pattern = "^\\./", ""
  )
}
