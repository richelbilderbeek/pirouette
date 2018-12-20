#' Creates a set of parameters used in the \link[babette]{babette} inference
#' @inheritParams default_params_doc
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
