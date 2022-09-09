#' Check if the \code{tree_and_model_errors} is valid.
#'
#' Check if the \code{tree_and_model_errors} is valid, will \link{stop} if not.
#'
#' A \code{tree_and_model_errors} must be a \link[tibble]{tibble}
#' with two columns, named \code{tree_and_model} and \code{error_value},
#' of which \code{tree_and_model} must be a factor.
#' @inheritParams default_params_doc
#' @return nothing
#' @author Richèl J.C. Bilderbeek
#' @export
check_tree_and_model_errors <- function(tree_and_model_errors) {
  if (!tibble::is_tibble(tree_and_model_errors)) {
    stop("'tree_and_model_errors' must be a tibble")
  }
  if (ncol(tree_and_model_errors) != 2) {
    stop("'tree_and_model_errors' must have 2 columns")
  }
  if (!"tree_and_model" %in% names(tree_and_model_errors)) {
    stop("'tree_and_model_errors' must have a column named 'tree_and_model'")
  }
  if (!"error_value" %in% names(tree_and_model_errors)) {
    stop("'tree_and_model_errors' must have a column named 'error_value'")
  }
  if (!is.factor(tree_and_model_errors$tree_and_model)) {
    stop("'tree_and_model_errors$tree_and_model' must be a factor")
  }
  invisible(tree_and_model_errors)
}
