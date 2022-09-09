#' Checks if BEAST2 is installed
#'
#' Will \link{stop} if not.
#' @author Richèl J.C. Bilderbeek
#' @export
check_beast2_installed <- function() {
  if (!beastier::is_beast2_installed()) {
    stop("BEAST2 not installed. Tip: use 'beastierinstall::install_beast2()'")
  }
}
