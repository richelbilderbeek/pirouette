#' Get the name for a temporary file to store the
#' evidences (aka marginal likelihoods) as
#' a comma-separated file
#' @export
get_temp_evidence_filename <- function() {
  tempfile(
    pattern = "evidence_",
    tmpdir = rappdirs::user_cache_dir(),
    fileext = ".csv"
  )
}
