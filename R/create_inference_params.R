#' Creates a set of parameters used in the Bayesian inference.
#' @inheritParams default_params_doc
#' @param rng_seed the random number generator seed
#'   used in the Bayesian inference.
#'   The Bayesian inference is handled by the \link[babette]{babette}
#'   R package, that calls the phylogenetic tool \code{BEAST2}.
#' @author Richel J.C. Bilderbeek
#' @export
create_inference_params <- function(
  model_selection = "generative",
  site_model = beautier::create_jc69_site_model(),
  clock_model = beautier::create_strict_clock_model(),
  tree_prior = beautier::create_bd_tree_prior(),
  mrca_prior = NA,
  mcmc = beautier::create_mcmc(),
  rng_seed = NA,
  beast2_path = beastier::get_default_beast2_path(),
  verbose = FALSE
) {
  inference_params <- list(
    model_selection = model_selection,
    site_model = site_model,
    clock_model = clock_model,
    tree_prior = tree_prior,
    mrca_prior = mrca_prior,
    mcmc = mcmc,
    rng_seed = rng_seed,
    beast2_path = beast2_path,
    verbose = verbose
  )
  check_inference_params(inference_params) # nolint pirouette function
  inference_params
}
