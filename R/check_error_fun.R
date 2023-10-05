#' Check that the \code{error_fun} is valid.
#'
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @return nothing
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Check cleanup by other functions
#' beastier::check_empty_beaustier_folders()
#'
#' check_error_fun(get_gamma_error_fun())
#' check_error_fun(get_nltt_error_fun())
#'
#' @export
check_error_fun <- function(error_fun) {
  # check if error_fun is indeed a function
  if (!is.function(error_fun)) {
    stop("'error_fun' must be a function")
  }

  # check if error_fun is indeed a function with at least 2 parameters
  arguments <- utils::capture.output(
    utils::str(args(error_fun))
  )
  n_commas <- stringr::str_count(string = arguments, pattern = ",")
  if (!(n_commas > 0)) {
    stop(
      "'error_fun' must be a function with at least two arguments"
    )
  }
  # check if error_fun is indeed a function that has a lowest
  # value for identical trees
  tree <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  trees <- c(tree)
  test_errors <- error_fun(
    tree = tree,
    trees = trees
  )
  if (!all(test_errors == 0.0)) {
    stop(
      "'error_fun' must be a function that is zero for identical trees"
    )
  }
  invisible(error_fun)
}
