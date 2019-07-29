#' Convert multiple filenames to their twin equivalent
#' @inheritParams default_params_doc
#' @export
to_twin_filenames <- function(filenames) {
  for (i in seq_along(filenames)) {
    filenames[i] <- to_twin_filename(filenames[i])
  }
  filenames
}
