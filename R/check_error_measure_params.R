#' Checks if the argument is a valid error_measure parameters structure,
#' as created by \link{create_error_measure_params}.
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if nit
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @examples
#' library(testthat)
#'
#' expect_silent(check_error_measure_params(create_error_measure_params()))
#' expect_error(check_error_measure_params("nonsense"))
#' expect_error(check_error_measure_params(NA))
#' expect_error(check_error_measure_params(NULL))
#' @export
check_error_measure_params <- function(
  error_measure_params
) {
  argument_names <- c(
    "burn_in_fraction",
    "error_function"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(error_measure_params)) {
      stop(
        "'", arg_name, "' must be an element of an 'error_measure_params'. ",
        "Tip: use 'create_error_measure_params'"
      )
    }
  }
  if (!beautier::is_one_double(error_measure_params$burn_in_fraction)) {
    stop("'burn_in_fraction' must be a number")
  }
  if (error_measure_params$burn_in_fraction < 0.0 ||
      error_measure_params$burn_in_fraction > 1.0) {
    stop("'burn_in_fraction' must be between 0.0 and 1.0")
  }

  pirouette::check_error_function(
    error_measure_params$error_function
  )

}
