#' Creates a set of parameters used in the Bayesian inference.
#' @inheritParams default_params_doc
#' @param rng_seed the random number generator seed
#'   used in the Bayesian inference.
#'   The Bayesian inference is handled by the \link[babette]{babette}
#'   R package, that calls the phylogenetic tool \code{BEAST2}.
#' @author Richel J.C. Bilderbeek
#' @export
create_inference_params <- function(
  mcmc,
  rng_seed = 0
) {
  inference_params <- list(
    mcmc = mcmc,
    rng_seed = rng_seed
  )
  check_inference_params(inference_params) # nolint pirouette function
  inference_params
}
