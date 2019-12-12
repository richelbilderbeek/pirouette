#' Get the name for a temporary file to store
#' inference errors.
#' @export
get_temp_errors_filename <- function() {
  tempfile(
    pattern = "errors_",
    tmpdir = rappdirs::user_cache_dir(),
    fileext = ".csv"
  )
}
