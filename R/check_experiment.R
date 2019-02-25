#' Checks if the argument is a valid \link{pirouette} experiment.
#'
#' Will \link{stop} if not.
#' A valid \link{pirouette} experiment
#' can be created by \link{create_experiment}.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if not
#' @examples
#'  experiment <- create_experiment()
#'  testthat::expect_silent(check_experiment(create_experiment()))
#' @author Richel J.C. Bilderbeek
#' @export
check_experiment <- function(
  experiment
) {
  argument_names <- c(
    "model_type",
    "run_if",
    "do_measure_evidence",
    "inference_model",
    "beast2_options",
    "est_evidence_mcmc",
    "beast2_bin_path",
    "errors_filename"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(experiment)) {
      stop(
        "'", arg_name, "' must be an element of an 'experiment'. ",
        "Tip: use 'create_experiment'"
      )
    }
  }
  if (!experiment$model_type %in% c("generative", "candidate")) {
    stop("'model_type' must be either \"generative\" or \"candidate\"")
  }
  if (!experiment$run_if %in% c("always", "best_candidate")) {
    stop("'run_if' must be either \"always\" or \"best_candidate\"")
  }
  if (!experiment$do_measure_evidence %in% c(TRUE, FALSE)) {
    stop("'do_measure_evidence' must be either TRUE or FALSE")
  }
  tryCatch(
    beautier::check_inference_model(experiment$inference_model),
    error = function(e) {
      stop(
        "'inference_model' must be a valid inference model\n",
        "Tip: use 'beautier::create_inference_model'.\n",
        "Error: ", e$message, "\n",
        "Value: ", experiment$inference_model
      )
    }
  )
  if (beautier::is_nested_sampling_mcmc(experiment$inference_model$mcmc)) {
    stop(
      "An experiment's inference model must have a regular MCMC.\n",
      "Tip: use 'beautier::create_mcmc'\n",
      "Value: ", experiment$inference_model$mcmc
    )
  }

  tryCatch(
    beastier::check_beast2_options(experiment$beast2_options),
    error = function(e) {
      stop(
        "'beast2_options' must be valid BEAST2 options.\n",
        "Tip: use 'beastier::create_beast2_options'.\n",
        "Error: ", e$message, "\n",
        "Value: ", experiment$beast2_options
      )
    }
  )
  if (!beautier::is_nested_sampling_mcmc(experiment$est_evidence_mcmc)) {
    stop(
      "'est_evidence_mcmc' must be a Nested Sampling MCMC.\n",
      "Tip: use 'beautier::create_nested_sampling_mcmc'\n",
      "Value: ", experiment$est_evidence_mcmc
    )
  }
  if (!beastier::is_bin_path(experiment$beast2_bin_path)) {
    stop(
      "'beast2_bin_path' must be a path to a BEAST2 binary file.\n",
      "Tip: use 'beastier::get_default_beast2_bin_path'\n",
      "Value: ", experiment$beast2_bin_path
    )
  }
  if (experiment$run_if == "best_candidate" &&
      experiment$do_measure_evidence == FALSE) {
    stop(
      "'run_if' == 'best_candidate' and 'do_measure_evidence' == FALSE ",
      "is a configuration that makes no sense"
    ) # nolint
  }
  if (!is.character(experiment$errors_filename)) {
    stop("'errors_filename' must be a character vector")
  }
  if (tools::file_ext(experiment$errors_filename) != "csv") {
    stop("'errors_filename' must be a csv file")
  }
}
