#' Create a valid testing \link{pirouette} experiment.
#' @inheritParams default_params_doc
#' @return a \link{pirouette} experiment.
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' experiment <- create_test_experiment()
#'
#' expect_true("inference_conditions" %in% names(experiment))
#' expect_true("inference_model" %in% names(experiment))
#' expect_true("beast2_options" %in% names(experiment))
#' expect_true("est_evidence_mcmc" %in% names(experiment))
#' expect_true("beast2_bin_path" %in% names(experiment))
#'
#' expect_silent(check_experiment(experiment))
#' @export
create_test_experiment <- function(
  inference_conditions = create_inference_conditions(),
  inference_model = beautier::create_inference_model(
    mcmc = beautier::create_mcmc(chain_length = 2000, store_every = 1000)
  ),
  beast2_options = beastier::create_beast2_options()
) {
  create_experiment(
    inference_conditions = inference_conditions,
    inference_model = inference_model,
    beast2_options = beast2_options
  )
}
