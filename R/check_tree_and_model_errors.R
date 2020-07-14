#' Plot the error 'BEAST2' makes from a known phylogeny
#' from tidy data
#' @inheritParams default_params_doc
#' @return a \code{ggplot2} plot
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
}
