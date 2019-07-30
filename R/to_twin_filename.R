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
  if (!beautier::is_one_string(filename)) {
    stop(
      "'filename' must be one string. \n",
      "Actual value: ", filename
    )
  }
  testit::assert(beautier::is_one_string(filename))
  # Get the basename with extension
  base_filename <- NA
  tryCatch({
      base_filename <- basename(filename)
    },
    error = function(e) {
      stop(
        "Cannot take basename of filename '", filename, "' \n",
        "of class '", class(filename), "' \n",
        "Error: ", e$message
      )
    }
  )
  testit::assert(is.character(base_filename))

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

  testit::assert(twin_path != filename)

twin_path
}
