#' Converts true tree filenames to twin tree filenames
#' @inheritParams default_params_doc
#' @return twin tree filename
#' @export
#' @author Giovanni Laudanno
to_twin_filename <- function(
  filename
) {
  stringr::str_replace(string = filename, pattern = "\\.", "_twin.")
}
