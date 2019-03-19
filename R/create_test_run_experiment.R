#' Create a run experiment for testing
#'
#' A run experiment is an extension of a normal experiment (that
#' has not been run), as it has added:
#' \itemize{
#'   \item log_evidence the natural logarithm of the evidence (aka marginal
#'     likelihood). Can be NA if this is not measured
#'   \item weight the weight of the model, compared to other (candidate)
#'     models. This weight will be between 0.0 (there is no evidence for
#'     this model) to 1.0 (all evidence indicates this is the best model).
#'     A weight of NA denotes that the weight is not measured
#'   \item errors a numeric vector of (positive) Bayesian inference errors.
#'     Will be NA if these are not measured.
#' }
#' @inheritParams default_params_doc
#' @return a run experiment
#' @author Richel J.C. Bilderbeek
#' @examples
#'   testthat::expect_silent(
#'     check_run_experiment(create_test_run_experiment())
#'   )
#' @export
create_test_run_experiment <- function(
  log_evidence = -123.456,
  weight = 0.5,
  errors = c(0.3, 0.1, 0.4, 0.1, 0.5, 0.9, 0.3)
) {
  run_experiment <- create_test_experiment()
  run_experiment$log_evidence <- log_evidence
  run_experiment$weight <- weight
  run_experiment$errors <- errors
  check_run_experiment(run_experiment)
  run_experiment
}
