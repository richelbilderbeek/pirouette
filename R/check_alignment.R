#' Check if the alignment is of the right type
#'
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @return nothing
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Cleanup
#' beastier::remove_beaustier_folders()
#' beastier::check_empty_beaustier_folders()
#'
#' # Check cleanup by other functions
#' beastier::check_empty_beaustier_folders()
#'
#' check_alignment(ape::as.DNAbin("ACGT"))
#'
#' # Cleanup
#' beastier::remove_beaustier_folders()
#' beastier::check_empty_beaustier_folders()
#' @export
check_alignment <- function(alignment) {
  if (!inherits(alignment, "DNAbin")) {
    stop("'alignment' must be of class 'ape::DNAbin'")
  }
  invisible(alignment)
}
