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
    input_filename = tempfile(pattern = "beast2_", fileext = ".xml"),
    output_log_filename = tempfile(pattern = "beast2_", fileext = ".log"),
    output_trees_filenames = tempfile(pattern = "beast2_", fileext = "trees"),
    output_state_filename = tempfile(
      pattern = "beast2_", fileext = ".state.xml"
    )
  ),
  est_evidence_mcmc = beautier::create_nested_sampling_mcmc(),
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

  check_experiment(experiment) # nolint pirouette function
  experiment
}

#' Create a valid testing \link{pirouette} experiment.
#' @inheritParams default_params_doc
#' @return a \link{pirouette} experiment.
#' @author Richèl J.C. Bilderbeek
#' @examples
#'  library(testthat)
#'
#'  experiment <- create_test_experiment()
#'
#'  expect_true("inference_conditions" %in% names(experiment))
#'  expect_true("inference_model" %in% names(experiment))
#'  expect_true("beast2_options" %in% names(experiment))
#'  expect_true("est_evidence_mcmc" %in% names(experiment))
#'  expect_true("beast2_bin_path" %in% names(experiment))
#'
#'  expect_silent(check_experiment(experiment))
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

#' Create a valid testing \link{pirouette} candidate experiment.
#' @inheritParams default_params_doc
#' @return a \link{pirouette} experiment.
#' @author Richèl J.C. Bilderbeek
#' @examples
#'  library(testthat)
#'
#'  experiment <- create_test_cand_experiment()
#'
#'  expect_true("inference_conditions" %in% names(experiment))
#'  expect_true("inference_model" %in% names(experiment))
#'  expect_true("beast2_options" %in% names(experiment))
#'  expect_true("est_evidence_mcmc" %in% names(experiment))
#'  expect_true("beast2_bin_path" %in% names(experiment))
#'
#'  expect_silent(check_experiment(experiment))
#' @export
create_test_cand_experiment <- function(
  inference_conditions = create_inference_conditions(
    model_type = "candidate",
    run_if = "best_candidate",
    do_measure_evidence = TRUE
  ),
  inference_model = pirouette::create_inference_model(
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

#' Create a valid testing \link{pirouette} generative experiment.
#' @inheritParams default_params_doc
#' @return a \link{pirouette} experiment.
#' @author Richèl J.C. Bilderbeek
#' @examples
#'  library(testthat)
#'
#'  # Create a testing candidate experiment
#'  experiment <- create_test_cand_experiment()
#'  expect_true("inference_conditions" %in% names(experiment))
#'  expect_true("inference_model" %in% names(experiment))
#'  expect_true("beast2_options" %in% names(experiment))
#'  expect_true("est_evidence_mcmc" %in% names(experiment))
#'  expect_true("beast2_bin_path" %in% names(experiment))
#'  expect_silent(check_experiment(experiment))
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
#'  # Use the experiment to create the full pirouette parameter set
#'  pir_params <- create_pir_params(
#'    alignment_params = create_test_alignment_params(),
#'    experiments = list(experiment)
#'  )
#'
#'  # Run that experiment on a continuous integration service,
#'  # only when BEAST2 is unstalled
#'  if (is_on_ci() && is_beast2_installed()) {
#'    pir_out <- pir_run(
#'      phylogeny = ape::read.tree(text = "((A:2, B:2):1, C:3);"),
#'      pir_params = pir_params
#'    )
#'    pir_plot(pir_out)
#'  }
#' @export
create_test_gen_experiment <- function(
  inference_conditions = create_inference_conditions(),
  inference_model = pirouette::create_inference_model(
    mcmc = beautier::create_mcmc(chain_length = 2000, store_every = 1000)
  ),
  beast2_options = beastier::create_beast2_options()
) {
  create_test_experiment( # nolint pirouette function
    inference_conditions = inference_conditions,
    inference_model = inference_model,
    beast2_options = beast2_options
  )
}
