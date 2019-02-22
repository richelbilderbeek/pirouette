#' Create a valid \link{pirouette} experiment.
#'
#' The arguments are checked by \link{check_experiment}.
#' @inheritParams default_params_doc
#' @return a \link{pirouette} experiment.
#' @examples
#'  experiment <- create_experiment()
#'  testthat::expect_silent(check_experiment(create_experiment()))
#'  testit::assert("model_type" %in% names(experiment))
#'  testit::assert("run_if" %in% names(experiment))
#'  testit::assert("do_measure_evidence" %in% names(experiment))
#'  testit::assert("inference_model" %in% names(experiment))
#'  testit::assert("beast2_options" %in% names(experiment))
#'  testit::assert("est_evidence_mcmc" %in% names(experiment))
#'  testit::assert("beast2_bin_path" %in% names(experiment))
#' @export
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
create_experiment <- function(
  model_type = "generative",
  run_if = "always",
  do_measure_evidence = FALSE,
  inference_model = beautier::create_inference_model(),
  beast2_options = beastier::create_beast2_options(),
  est_evidence_mcmc = beautier::create_nested_sampling_mcmc(),
  beast2_bin_path = beastier::get_default_beast2_bin_path()
) {
  if (rappdirs::app_dir()$os == "win" && do_measure_evidence == TRUE) {
    stop("This configuration cannot run on windows")
  }
  experiment <- list(
    model_type = model_type,
    run_if = run_if,
    do_measure_evidence = do_measure_evidence,
    inference_model = inference_model,
    beast2_options = beast2_options,
    est_evidence_mcmc = est_evidence_mcmc,
    beast2_bin_path = beast2_bin_path
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
    )
  )
}
