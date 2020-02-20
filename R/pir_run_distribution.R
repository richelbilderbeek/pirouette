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
  # checks
  if (!is.list(pir_paramses)) {
    stop("'pir_paramses' must be a list of pir_params objects")
  }
  if (length(pir_paramses) == 0) {
    stop("'pir_paramses' must be a list of pir_params objects")
  }
  for (i in seq_along(pir_paramses)) {
    tryCatch(
      pirouette::check_pir_params(pir_paramses[[i]]),
      error = function(e) {
        stop("'pir_paramses' must be a list of pir_params objects")
      }
    )
  }
  if (!is.function(sim_phylo_fun)) {
    stop("'sim_phylo_fun' must be a function")
  }
  if (!beautier::is_phylo(sim_phylo_fun())) {
    stop("'sim_phylo_fun' must be a function that returns an ape::phylo object")
  }

  # simulate trees
  pir_outs <- l_parses <- length(pir_paramses)
  phylogenies <- vector("list", l_parses)
  for (seed in seq_along(pir_paramses)) {
    set.seed(seed)
    phylogenies[[seed]] <- sim_phylo_fun()
  }

  # pir run!
  pir_outs <- pirouette::pir_runs(
    phylogenies = phylogenies,
    pir_paramses = pir_paramses
  )

  pir_outs
}
