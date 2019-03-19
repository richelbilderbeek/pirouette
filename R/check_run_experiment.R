#' Check if the experiment is a run experiment
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if not
#' @author Richel J.C. Bilderbeek
#' @examples
#'   testthat::expect_silent(
#'     check_run_experiment(create_test_run_experiment())
#'   )
#' @export
check_run_experiment <- function(run_experiment) {
  check_experiment(run_experiment)
  argument_names <- c(
    "log_evidence",
    "weight",
    "errors"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(run_experiment)) {
      stop(
        "'", arg_name, "' must be an element of a 'run_experiment'"
      )
    }
  }
}
