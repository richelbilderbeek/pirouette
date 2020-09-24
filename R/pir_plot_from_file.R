#' Plot the error BEAST2 make from the known phylogeny
#' @param pir_out_filename name of the file with the saved
#' output as created by \code{\link{pir_run}}
#' @return a ggplot2 plot
#' @seealso
#'   Use \link{pir_plot} to directly plot the return value
#'   of \link{pir_run}
#' @author Richèl J.C. Bilderbeek
#' @export
pir_plot_from_file <- function(pir_out_filename) {

  pir_out <- utils::read.csv(pir_out_filename)
  pirouette::pir_plot(pir_out)
}
