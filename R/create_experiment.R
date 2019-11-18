#' Create a valid \link{pirouette} experiment.
#'
#' The arguments are checked by \link{check_experiment}.
#' @inheritParams default_params_doc
#' @return a \link{pirouette} experiment.
#' @examples
#'  library(testthat)
#'
#'  experiment <- create_experiment()
#'
#'  expect_true("inference_conditions" %in% names(experiment))
#'  expect_true("inference_model" %in% names(experiment))
#'  expect_true("beast2_options" %in% names(experiment))
#'  expect_true("est_evidence_mcmc" %in% names(experiment))
#'  expect_true("beast2_bin_path" %in% names(experiment))
#'
#'  expect_silent(check_experiment(experiment))
#' @export
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
create_experiment <- function(
  inference_conditions = create_inference_conditions(),
  inference_model = beautier::create_inference_model(
    mcmc = beautier::create_mcmc(store_every = 1000)
  ),
  beast2_options = beastier::create_beast2_options(
    input_filename = beastier::create_temp_input_filename(),
    output_state_filename = beastier::create_temp_state_filename()
  ),
  est_evidence_mcmc = beautier::create_nested_sampling_mcmc(epsilon = 1e-12),
  beast2_bin_path = beastier::get_default_beast2_bin_path(),
  errors_filename = tempfile(pattern = "errors_", fileext = ".csv")
) {
  experiment <- list(
    inference_conditions = inference_conditions,
    inference_model = inference_model,
    beast2_options = beast2_options,
    est_evidence_mcmc = est_evidence_mcmc,
    beast2_bin_path = beast2_bin_path,
    errors_filename = errors_filename
  )

  pirouette::check_experiment(experiment)
  experiment
}
