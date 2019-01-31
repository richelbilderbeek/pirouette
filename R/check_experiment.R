#' Checks if the argument is a valid \link{pirouette} experiment.
#'
#' Will \link{stop} if not.
#' A valid \link{pirouette} experiment
#' can be created by \link{create_experiment}.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if not
#' @author Richel J.C. Bilderbeek
check_experiment <- function(
  experiment
) {
  argument_names <- c(
    "model_type", "run_if", "do_measure_evidence",  "inference_model",
    "beast2_options"
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
  beautier::check_inference_model(experiment$inference_model)
  beastier::check_beast2_options(experiment$beast2_options)
}
