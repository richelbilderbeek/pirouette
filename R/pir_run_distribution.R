#' Do multiple \link{pirouette} runs off a distribition of phylogenies
#'
#' This is a simple convenience functions: supply a phylogeny generator
#' function and one or more  \link{pirouette} parameter sets.
#' For each parameters pair, \link{pir_run} is called with a phylogeny
#' drawn from the phylogeny generator function.
#' @inheritParams default_params_doc
#' @return a list of \link{pir_run} outputs.
#' @export
pir_run_distribution <- function(
  sim_phylo_fun,
  pir_paramses
) {

}
