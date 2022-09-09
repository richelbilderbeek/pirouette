#' Checks if the \code{NS} BEAST2 package is installed.
#'
#' Will \link{stop} if not.
#' @return nothing
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(babette)
#'
#' # We need BEAST2 installed
#' if (is_beast2_installed()) {
#'
#'   if (is_beast2_ns_pkg_installed()) {
#'     # No error If NS is installed
#'     check_is_ns_beast2_pkg_installed()
#'   }
#' }
#' @export
check_is_ns_beast2_pkg_installed <- function() { # nolint long function name indeed
  testthat::expect_true(beastier::is_beast2_installed())
  # Catch and ignore errors caused by connection problems, e.g:
  #
  # Error reading the following package repository URLs:
  #   https://raw.githubusercontent.com/CompEvol/CBAN/master/packages2.5.xml
  # Could not get an internet connection.
  # The BEAST Package Manager needs internet access in order to list
  # available packages and download them for installation.
  # Possibly, some software (like security software, or a firewall)
  # blocks the BEAST Package Manager.
  # If so, you need to reconfigure such software to allow access.
  tryCatch({
      if (!mauricer::is_beast2_ns_pkg_installed()) {
        stop(
          "BEAST2 package 'NS' is not installed\n",
          "Tip: use 'mauricerinstall::install_beast2_pkg(\"NS\")'\n"
        )
      }
    }, error = function() {} # nolint we ignore the error
  )
  invisible(NULL)
}
