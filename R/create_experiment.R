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
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
create_experiment <- function(
  inference_conditions = create_inference_conditions(),
  inference_model = beautier::create_inference_model(
    mcmc = create_mcmc(store_every = 1000)
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
#' @export
#' @author Richel J.C. Bilderbeek
create_test_experiment <- function() {
  create_experiment(
    inference_model = create_inference_model(
      mcmc = create_mcmc(chain_length = 2000, store_every = 1000)
    ),
    beast2_options = create_beast2_options(
      input_filename = tempfile(pattern = "beast2_", fileext = ".xml"),
      output_log_filename = tempfile(pattern = "beast2_", fileext = ".log"),
      output_trees_filenames = tempfile(pattern = "beast2_", fileext = "trees"),
      output_state_filename = tempfile(
        pattern = "beast2_", fileext = ".state.xml"
      )
    )
  )
}

#' Create a valid testing \link{pirouette} candidate experiment.
#' @inheritParams default_params_doc
#' @return a \link{pirouette} experiment.
#' @export
#' @author Richel J.C. Bilderbeek
create_test_cand_experiment <- function() {
  create_experiment(
    inference_conditions = create_inference_conditions(
      model_type = "candidate",
      run_if = "best_candidate",
      do_measure_evidence = TRUE
    ),
    inference_model = create_inference_model(
      mcmc = create_mcmc(chain_length = 2000, store_every = 1000)
    ),
    beast2_options = create_beast2_options(
      input_filename = tempfile(pattern = "beast2_", fileext = ".xml"),
      output_log_filename = tempfile(pattern = "beast2_", fileext = ".log"),
      output_trees_filenames = tempfile(pattern = "beast2_", fileext = "trees"),
      output_state_filename = tempfile(
        pattern = "beast2_", fileext = ".state.xml"
      )
    ),
    est_evidence_mcmc = create_nested_sampling_mcmc(
      chain_length = 2000,
      store_every = 1000,
      epsilon = 100.0
    )
  )
}

#' Create a valid testing \link{pirouette} generative experiment.
#' @inheritParams default_params_doc
#' @return a \link{pirouette} experiment.
#' @export
#' @author Richel J.C. Bilderbeek
create_test_gen_experiment <- function() {
  create_test_experiment()
}
