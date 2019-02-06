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
  beautier::check_mcmc(mcmc)
  beautier::check_mrca_prior(mrca_prior)
  inference_params <- list(
    mrca_prior = mrca_prior,
    mcmc = mcmc,
    rng_seed = rng_seed,
    beast2_path = beast2_path,
    verbose = verbose
  )
  check_inference_params(inference_params) # nolint pirouette function
  inference_params
}
