#' Checks if the argument is a valid error_measure parameters structure,
#' as created by \link{create_error_measure_params}.
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if nit
#' @author Richel J.C. Bilderbeek
check_error_measure_params <- function(
  error_measure_params
) {
  argument_names <- c(
    "burn_in_fraction", "error_function"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(error_measure_params)) {
      stop(
        "'", arg_name, "' must be an element of an 'error_measure_params'. ",
        "Tip: use 'create_error_measure_params'"
      )
    }
  }
  if (!is.numeric(error_measure_params$burn_in_fraction)) {
    stop("'burn_in_fraction' must be a number")
  }
  if (error_measure_params$burn_in_fraction < 0.0 ||
      error_measure_params$burn_in_fraction > 1.0) {
    stop("'burn_in_fraction' must be between 0.0 and 1.0")
  }
  # TODO: check if error_function is indeed a function
  # TODO: check if error_function is indeed a function that has a lowest
  #       value for identical trees
}
