#' Checks if the argument is a list of one or more \link{pirouette} experiments.
#'
#' Will \link{stop} if not.
#' A valid \link{pirouette} experiment
#' can be created by \link{create_experiment}.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if not
#' @examples
#'  experiments <- list(create_experiment())
#'  testthat::expect_silent(check_experiments(experiments))
#' @author Richel J.C. Bilderbeek
#' @export
check_experiments <- function(
  experiments
) {
  if (!is.list(experiments)) {
    stop(
      "'experiments' must be a list of one or more experiments.\n",
      "Actual value: ", experiments
    )
  }
  for (i in seq_along(experiments)) {
    experiment <- experiments[[i]]
    tryCatch(
      check_experiment(experiment), # nolint pirouette function
      error = function(e) {
        stop(
          "'experiments[[", i, "]] invalid.\n",
          "Tip: use 'create_experiment' for each list element.\n",
          "Error: ", e$message, "\n",
          "Value: ", experiment
        )
      }
    )
  }
}
