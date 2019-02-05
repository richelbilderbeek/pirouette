#' Checks if the \code{NS} BEAST2 package is installed.
#'
#' Will \link{stop} if not.
#' @author Richel J.C. Bilderbeek
#' @export
check_is_ns_beast2_pkg_installed <- function() {
  if (!mauricer::is_beast2_pkg_installed("NS")) {
    stop(
      "BEAST2 package 'NS' is not installed\n",
      "Tip: use 'mauricer::install_beast2_pkg(\"NS\")'\n"
    )
  }
}
