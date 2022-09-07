#' Check if the alignment is of the right type
#'
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @export
check_alignment <- function(alignment) {
  if (!inherits(alignment, "DNAbin")) {
    stop("'alignment' must be of class 'ape::DNAbin'")
  }
}
