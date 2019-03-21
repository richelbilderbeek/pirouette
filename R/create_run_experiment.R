#' Create a valid \link{pirouette} run experiment.
#'
#' The arguments are checked by \link{check_run_experiment}.
#' @inheritParams default_params_doc
#' @return a \link{pirouette} run experiment.
#' @export
#' @author Richel J.C. Bilderbeek
create_run_experiment <- function(
  experiment
) {
  run_experiment <- list(
    experiment = experiment,
    true_results = NA,
    twin_results = NA
  )
  check_run_experiment(run_experiment) # nolint pirouette function
  run_experiment
}

#' Create a valid testing \link{pirouette} run experiment
#' @inheritParams default_params_doc
#' @return a \link{pirouette} run experiment.
#' @export
#' @author Richel J.C. Bilderbeek
create_test_run_experiment <- function() {
  create_run_experiment(
    create_test_experiment()
  )
}
