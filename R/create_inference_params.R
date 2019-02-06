#' Creates the parameters all BEAST2 runs share.
#'
#' @inheritParams default_params_doc
#' @param rng_seed the random number generator seed
#'   used in the Bayesian inference.
#'   The Bayesian inference is handled by the \link[babette]{babette}
#'   R package, that calls the phylogenetic tool \code{BEAST2}.
#' @author Richel J.C. Bilderbeek
#' @export
create_inference_params <- function(
  mrca_prior = NA,
  mcmc = beautier::create_mcmc(),
  rng_seed = NA,
  beast2_path = beastier::get_default_beast2_path(),
  verbose = FALSE
) {
  stop("DEPORWIUFGOE")
}
