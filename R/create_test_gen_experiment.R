#' Create a valid testing \link{pirouette} generative experiment.
#' @inheritParams default_params_doc
#' @return a \link{pirouette} experiment.
#' @author Richèl J.C. Bilderbeek
#' @examples
#'  # Create a testing candidate experiment
#'  if (rappdirs::app_dir()$os != "win") {
#'    create_test_cand_experiment()
#'  }
#'
#'  # Create a testing generative experiment
#'  create_test_gen_experiment()
#' @export
create_test_gen_experiment <- function(
  inference_conditions = create_inference_conditions(),
  inference_model = beautier::create_test_inference_model(),
  beast2_options = beastier::create_beast2_options(),
  est_evidence_mcmc = beautier::create_test_ns_mcmc(),
  beast2_bin_path = beastier::get_default_beast2_bin_path(),
  errors_filename = get_temp_errors_filename()
) {
  pirouette::create_test_experiment(
    inference_conditions = inference_conditions,
    inference_model = inference_model,
    beast2_options = beast2_options,
    est_evidence_mcmc = est_evidence_mcmc,
    beast2_bin_path = beast2_bin_path,
    errors_filename = errors_filename
  )
}
