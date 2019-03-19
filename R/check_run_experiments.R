#' Checks if the argument is a list of one or more
#' \link{pirouette} run experiments.
#'
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if not
#' @seealso Use \link{check_run_experiment} to check if an object
#'   is one run experiment
#' @examples
#'   testthat::expect_silent(
#'     check_run_experiments(
#'       list(create_test_run_experiment())
#'     )
#'   )
#'   testthat::expect_error(
#'     check_run_experiments(
#'       create_test_run_experiment()
#'     )
#'   )
#'   testthat::expect_error(check_run_experiments("nonsense"))
#'   testthat::expect_error(check_run_experiments(NA))
#'   testthat::expect_error(check_run_experiments(NULL))
#' @author Richel J.C. Bilderbeek
#' @export
check_run_experiments <- function(
  run_experiments
) {
  if (!is.list(run_experiments)) {
    stop(
      "'experiments' must be a list of one or more run experiments.\n",
      "Actual value: ", run_experiments
    )
  }
  for (i in seq_along(run_experiments)) {
    run_experiment <- run_experiments[[i]]
    tryCatch(
      check_run_experiment(run_experiment), # nolint pirouette function
      error = function(e) {
        stop(
          "'run_experiments[[", i, "]] invalid.\n",
          "Error: ", e$message, "\n",
          "Value: ", run_experiment
        )
      }
    )
  }
}
