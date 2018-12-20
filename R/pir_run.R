#' Meaure the error BEAST2 makes from a known phylogeny
#' @inheritParams default_params_doc
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
