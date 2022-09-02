#' Converts true tree filenames to twin tree filenames
#' @inheritParams default_params_doc
#' @return twin tree filename
#' @export
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#'
#' filename <- "beast2_output.xml.state"
#' # beast2_output_twin.xml.state
#' to_twin_filename(filename)
to_twin_filename <- function(
  filename
) {
  beautier::check_filename(filename)
  # Get the basename with extension
  base_filename <- basename(filename)

  testthat::expect_silent(beautier::check_filename(base_filename))

  if (!stringr::str_count(base_filename, pattern = "\\.")) {
    twin_basename <- paste0(base_filename, "_twin")
  } else {
    # Replace the first dot with '_twin.'
    twin_basename <- stringr::str_replace(
      string = base_filename,
      pattern = "\\.", "_twin."
    )
  }

  # Complete the path
  twin_path <- file.path(
    dirname(filename),
    twin_basename
  )

  # Remove the './' at the beginning if present
  twin_path <- stringr::str_replace(
    string = twin_path,
    pattern = "^\\./", ""
  )

  testthat::expect_true(twin_path != filename)

twin_path
}
