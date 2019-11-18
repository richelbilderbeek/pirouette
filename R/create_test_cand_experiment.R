#' Create a valid testing \link{pirouette} candidate experiment.
#' @inheritParams default_params_doc
#' @return a \link{pirouette} experiment.
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' if (rappdirs::app_dir()$os != "win") {
#'   experiment <- create_test_cand_experiment()
#'
#'   expect_true("inference_conditions" %in% names(experiment))
#'   expect_true("inference_model" %in% names(experiment))
#'   expect_true("beast2_options" %in% names(experiment))
#'   expect_true("est_evidence_mcmc" %in% names(experiment))
#'   expect_true("beast2_bin_path" %in% names(experiment))
#'   expect_silent(check_experiment(experiment))
#' }
#' @export
create_test_cand_experiment <- function(
  inference_conditions = pirouette::create_inference_conditions(
    model_type = "candidate",
    run_if = "best_candidate",
    do_measure_evidence = TRUE
  ),
  inference_model = beautier::create_test_inference_model(),
  beast2_options = beastier::create_beast2_options(),
  est_evidence_mcmc = beautier::create_test_ns_mcmc(),
  beast2_bin_path = beastier::get_default_beast2_bin_path(),
  errors_filename = get_temp_errors_filename()
) {
  create_experiment(
    inference_conditions = inference_conditions,
    inference_model = inference_model,
    beast2_options = beast2_options,
    est_evidence_mcmc = est_evidence_mcmc,
    beast2_bin_path = beast2_bin_path,
    errors_filename = errors_filename
  )
}
