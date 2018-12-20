#' Creates a set of parameters used in the \link[babette]{babette} inference
#' @inheritParams default_params_doc
#' @param rng_seed the random number generator seed used in the
#'   \link[babette]{babette} inference
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
  check_inference_params(inference_params)
  inference_params
}
