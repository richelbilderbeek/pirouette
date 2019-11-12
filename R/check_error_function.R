#' Check that the \code{error_function} is valid.
#'
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @export
check_error_function <- function(error_function) {
  # check if error_function is indeed a function
  if (!is.function(error_function)) {
    stop("'error_function' must be a function")
  }

  # check if error_function is indeed a function with at least 2 parameters
  arguments <- utils::capture.output(
    utils::str(args(error_function))
  )
  n_commas <- stringr::str_count(string = arguments, pattern = ",")
  if (!(n_commas > 0)) {
    stop(
      "'error_function' must be a function with at least two arguments"
    )
  }
  # check if error_function is indeed a function that has a lowest
  # value for identical trees
  tree <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  trees <- c(tree)
  test_errors <- error_function(
    tree = tree,
    trees = trees
  )
  if (!all(test_errors == 0.0)) {
    stop(
      "'error_function' must be a function that is zero for identical trees"
    )
  }
}
