#' Create a run experiment for testing
#'
#' A run experiment is an extension of a normal experiment (that
#' has not been run), as it has added:
#' \itemize{
#'   \item true_result the results for the true tree
#'   \item twin_result the results for the twin tree
#' }
#' In both cases,
#' @inheritParams default_params_doc
#' @return a run experiment
#' @author Richel J.C. Bilderbeek
#' @examples
#'   library(testthat)
#'
#'   expect_silent(
#'     check_run_experiment(create_test_run_experiment())
#'   )
#' @export
create_test_run_experiment <- function(
  true_result = create_test_result(),
  twin_result = create_test_result()
) {
  run_experiment <- create_test_experiment() # nolint pirouette function
  run_experiment$true_result <- true_result
  run_experiment$twin_result <- twin_result
  check_run_experiment(run_experiment) # nolint pirouette function
  run_experiment
}
