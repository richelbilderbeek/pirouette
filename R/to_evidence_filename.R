#' Converts a filename to an evidence filename
#' @inheritParams default_params_doc
#' @return evidence tree filename
#' @export
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' filename <- "beast2_output.xml.state"
#' created <- to_evidence_filename(filename)
#' expected <- "beast2_output_evidence.xml.state"
#' expect_equal(expected, created)
to_evidence_filename <- function(
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
    evidence_basename <- paste0(base_filename, "_evidence")
  } else {
    # Replace the first dot with '_evidence.'
    evidence_basename <- stringr::str_replace(
      string = base_filename,
      pattern = "\\.", "_evidence."
    )
  }

  # Complete the path
  evidence_path <- file.path(
    dirname(filename),
    evidence_basename
  )

  # Remove the './' at the beginning if present
  evidence_path <- stringr::str_replace(
    string = evidence_path,
    pattern = "^\\./", ""
  )

  testit::assert(evidence_path != filename)

evidence_path
}