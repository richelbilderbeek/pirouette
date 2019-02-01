#' Checks if the argument is a valid error_measure parameters structure,
#' as created by \link{create_error_measure_params}.
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if nit
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
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
  if (!is.numeric(error_measure_params$burn_in_fraction)) {
    stop("'burn_in_fraction' must be a number")
  }
  if (error_measure_params$burn_in_fraction < 0.0 ||
      error_measure_params$burn_in_fraction > 1.0) {
    stop("'burn_in_fraction' must be between 0.0 and 1.0")
  }

  # check if error_function is indeed a function
  if (!is.function(error_measure_params$error_function)) {
    stop("'error_function' must be a function")
  }

  # check if error_function is indeed a function with at least 2 parameters
  arguments <- capture.output(str(args(error_measure_params$error_function)))
  n_commas <- stringr::str_count(string = arguments, pattern = ",")
  if (!(n_commas > 0)) {
    stop(
      "'error_function' must have at least two arguments"
    )
  }

  # check if error_function is indeed a function that has a lowest
  # value for identical trees
  set.seed(42)
  tess_sim <- TESS::tess.sim.taxa.age(
    n = 1,
    nTaxa = 10,
    age = 10,
    lambda = 0.33,
    mu = 0.1
  )
  trees <- rep(tess_sim, 1e3)
  tree <- tess_sim[[1]]
  class(trees) <- "multiPhylo"
  test_errors <- error_measure_params$error_function(
    tree = tree,
    trees = trees
  )
  if (!all(test_errors == 0)) {
    stop(
      "'error_function' should return no error if applied to identical trees" # nolint long string
    )
  }
}
