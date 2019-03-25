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
  check_experiment(run_experiment) # nolint pirouette function
  argument_names <- c(
    "true_result",
    "twin_result"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(run_experiment)) {
      stop(
        "'", arg_name, "' must be an element of a 'run_experiment'"
      )
    }
  }

  # STUB
  if (!beautier::is_one_na(run_experiment$true_result)) {
    check_result(run_experiment$true_result) # nolint pirouette function
  }
  # STUB
  if (!beautier::is_one_na(run_experiment$twin_result)) {
    check_result(run_experiment$twin_result) # nolint pirouette function
  }
}
