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
  inference_conditions = create_inf_conds(
    model_type = "candidate",
    run_if = "best_candidate",
    do_measure_evidence = TRUE
  ),
  inference_model = beautier::create_inference_model(
    mcmc = beautier::create_mcmc(chain_length = 2000, store_every = 1000)
  ),
  beast2_options = beastier::create_beast2_options(
    input_filename = tempfile(pattern = "beast2_", fileext = ".xml"),
    output_log_filename = tempfile(pattern = "beast2_", fileext = ".log"),
    output_trees_filenames = tempfile(pattern = "beast2_", fileext = "trees"),
    output_state_filename = tempfile(
      pattern = "beast2_", fileext = ".state.xml"
    )
  ),
  est_evidence_mcmc = beautier::create_nested_sampling_mcmc(
    chain_length = 2000,
    store_every = 1000,
    epsilon = 100.0
  )
) {
  create_experiment(
    inference_conditions = inference_conditions,
    inference_model = inference_model,
    beast2_options = beast2_options,
    est_evidence_mcmc = est_evidence_mcmc
  )
}
