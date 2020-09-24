#' Create a valid \link{pirouette} experiment.
#'
#' The arguments are checked by \link{check_experiment}.
#' @inheritParams default_params_doc
#' @return a \link{pirouette} experiment.
#' @examples
#' create_experiment()
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
  est_evidence_mcmc = beautier::create_ns_mcmc(epsilon = 1e-12),
  beast2_bin_path = beastier::get_default_beast2_bin_path(),
  errors_filename = pirouette::get_temp_errors_filename()
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
