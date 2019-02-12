#' Converts true tree filenames to twin tree filenames
#' @inheritParams default_params_doc
#' @return twin tree filename
#' @export
#' @author Giovanni Laudanno
to_twin_filename <- function(
  filename
) {
  x <- tools::file_path_sans_ext(filename)
  y <- tools::file_ext(filename)
  z <- paste0(x, "_twin.", y)
  z
}
