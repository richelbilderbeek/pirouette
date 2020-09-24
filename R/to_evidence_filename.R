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
  if (!assertive::is_a_string(filename)) {
    stop(
      "'filename' must be one string. \n",
      "Actual value: ", filename
    )
  }
  testit::assert(assertive::is_a_string(filename))
  # Get the basename with extension
  base_filename <- basename(filename)
  testit::assert(assertive::is_a_string(base_filename))

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
