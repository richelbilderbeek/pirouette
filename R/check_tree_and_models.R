#' Check if the \code{tree_and_model} is valid
#' @inheritParams default_params_doc
#' @return a \code{ggplot2} plot
#' @author Richèl J.C. Bilderbeek
#' @export
check_tree_and_models <- function(tree_and_models) {
  if (length(tree_and_models) == 0) {
    stop("'tree_and_models' must be one or more values")
  }
  if (!all(tree_and_models %in% pirouette::get_tree_and_model_values())
  ) {
    stop("'tree_and_models' has at least one invalid value")
  }
}
