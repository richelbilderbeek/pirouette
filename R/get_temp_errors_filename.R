#' Get the name for a temporary file to store
#' inference errors.
#' @return one string
#' @export
get_temp_errors_filename <- function() {
  beautier::get_beautier_tempfilename(
    pattern = "errors_",
    fileext = ".csv"
  )
}
