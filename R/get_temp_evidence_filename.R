#' Get the name for a temporary file to store the
#' evidences (aka marginal likelihoods) as
#' a comma-separated file
#' @return one string
#' @export
get_temp_evidence_filename <- function() {
  beautier::get_beautier_tempfilename(
    pattern = "evidence_",
    fileext = ".csv"
  )
}
