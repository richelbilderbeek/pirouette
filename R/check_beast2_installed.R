#' Checks if BEAST2 is installed
#'
#' Will \link{stop} if not.
#' @return nothing
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Check cleanup by other functions
#' beastier::check_empty_beaustier_folders()
#'
#' # Need to check if it is installed, else it will give the desired error
#' if (beastier::is_beast2_installed()) {
#'   check_beast2_installed()
#' }
#'
#' @export
check_beast2_installed <- function() {
  if (!beastier::is_beast2_installed()) {
    stop("BEAST2 not installed. Tip: use 'beastierinstall::install_beast2()'")
  }
  invisible(NULL)
}
