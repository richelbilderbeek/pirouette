#' Checks if the argument is a valid \link{pirouette} result.
#'
#' Will \link{stop} if not.
#' A valid \link{pirouette} result
#' can be created by \link{create_result}.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if not
#' @seealso Use \link{check_results} to check if an object
#'   is a list of results
#' @author Richel J.C. Bilderbeek
#' @examples
#'   testthat::expect_silent(
#'     check_result(create_test_result())
#'   )
#'   testthat::expect_error(check_result("nonsense"))
#'   testthat::expect_error(check_result(NA))
#'   testthat::expect_error(check_result(NULL))
#' @export
check_result <- function(
  result
) {
  argument_names <- c(
    "log_evidence",
    "weight",
    "errors"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(result)) {
      stop(
        "'", arg_name, "' must be an element of an 'result'. ",
        "Tip: use 'create_result'"
      )
    }
  }

}
