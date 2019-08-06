#' Plot the error BEAST2 make from the known phylogeny
#' @param pir_out_filename name of the file with the saved
#' output as created by \code{\link{pir_run}}
#' @return a ggplot2 plot
#' @seealso
#'   Use \link{pir_plot} to directly plot the return value
#'   of \link{pir_run}
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' pir_out_filename <- tempfile(fileext = ".csv")
#' url <- paste0(
#'   "https://raw.githubusercontent.com/richelbilderbeek/",
#'   "pirouette_example_3/master/example_3_314/errors.csv"
#' )
#' utils::download.file(url = url, destfile = pir_out_filename, quiet = TRUE)
#' expect_silent(pir_plot_from_file(pir_out_filename))
#' @export
pir_plot_from_file <- function(pir_out_filename) {

  pir_out <- utils::read.csv(pir_out_filename)
  pirouette::check_pir_out(pir_out)
  pirouette::pir_plot(pir_out)
}
