#' Create a valid testing \link{pirouette} generative experiment.
#' @inheritParams default_params_doc
#' @return a \link{pirouette} experiment.
#' @author Richèl J.C. Bilderbeek
#' @examples
#'  library(testthat)
#'
#'  # Create a testing candidate experiment
#'  if (rappdirs::app_dir()$os != "win") {
#'  experiment <- create_test_cand_experiment()
#'  expect_true("inference_conditions" %in% names(experiment))
#'  expect_true("inference_model" %in% names(experiment))
#'  expect_true("beast2_options" %in% names(experiment))
#'  expect_true("est_evidence_mcmc" %in% names(experiment))
#'  expect_true("beast2_bin_path" %in% names(experiment))
#'  expect_silent(check_experiment(experiment))
#'  }
#'
#'  # Create a testing generative experiment
#'  experiment <- create_test_gen_experiment()
#'  expect_true("inference_conditions" %in% names(experiment))
#'  expect_true("inference_model" %in% names(experiment))
#'  expect_true("beast2_options" %in% names(experiment))
#'  expect_true("est_evidence_mcmc" %in% names(experiment))
#'  expect_true("beast2_bin_path" %in% names(experiment))
#'  expect_silent(check_experiment(experiment))
#'
#'  # Use the generative experiment
#'  # to create the full pirouette parameter set
#'  pir_params <- create_pir_params(
#'    alignment_params = create_test_alignment_params(),
#'    experiments = list(experiment)
#'  )
#'
#'  # Run that experiment on a continuous integration service,
#'  # only when BEAST2 is unstalled
#'  if (rappdirs::app_dir()$os != "win" &&
#'    is_on_ci() &&
#'    is_beast2_installed()
#'  ) {
#'    pir_out <- pir_run(
#'      phylogeny = ape::read.tree(text = "((A:2, B:2):1, C:3);"),
#'      pir_params = pir_params
#'    )
#'    pir_plot(pir_out)
#'  }
#' @export
create_test_gen_experiment <- function(
  inference_conditions = create_inference_conditions(),
  inference_model = beautier::create_test_inference_model(),
  beast2_options = beastier::create_beast2_options(),
  est_evidence_mcmc = beautier::create_test_ns_mcmc(),
  beast2_bin_path = beastier::get_default_beast2_bin_path(),
  errors_filename = tempfile(
    pattern = "errors_",
    tmpdir = rappdirs::user_cache_dir(),
    fileext = ".csv"
  )
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
