#' Meaure the error BEAST2 makes from a known phylogeny
#' @param phylogeny a phylogeny
#' @param mcmc MCMC options, as created by \link[beautier]{create_mcmc}
#' @return a data frame with errors
#' @export
#' @author Richel J.C. Bilderbeek
pir_run <- function(
  phylogeny,
  mcmc
) {
  twin_tree <- create_twin_tree(phylogeny)
  create_pir_run_test_output()
}
