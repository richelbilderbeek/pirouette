#' Converts a filename to an evidence filename
#' @inheritParams default_params_doc
#' @return evidence tree filename
#' @export
#' @author Richèl J.C. Bilderbeek
#' @examples
#' filename <- "beast2_output.xml.state"
#' # beast2_output_evidence.xml.state
#' to_evidence_filename(filename)
to_evidence_filename <- function(
  filename
) {
  beautier::check_filename(filename)
  # Get the basename with extension
  base_filename <- basename(filename)
  testthat::expect_silent(beautier::check_filename(base_filename))

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

  testthat::expect_true(evidence_path != filename)

  evidence_path
}
