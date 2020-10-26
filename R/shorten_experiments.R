#' Shorten the experiments' runtime
#' @inheritParams default_params_doc
#' @export
shorten_experiments <- function(
  experiments
) {
  pirouette::check_experiments(experiments)
  for (i in seq_along(experiments)) {
    experiments[[i]]$inference_model$mcmc$chain_length <- 2000
    experiments[[i]]$inference_model$mcmc$store_every <- 1000
    experiments[[i]]$est_evidence_mcmc$chain_length <- 2000
    experiments[[i]]$est_evidence_mcmc$store_every <- 1000
    experiments[[i]]$est_evidence_mcmc$epsilon <- 100.0
  }
  experiments
}
