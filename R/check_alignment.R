#' Check if the alignment is of the right type
#'
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
check_alignment <- function(alignment) {
  if (class(alignment) != "DNAbin") {
    stop("'alignment' must be of class 'ape::DNAbin'")
  }
}
