#' Check if the \code{tree_and_model} is valid
#' @inheritParams default_params_doc
#' @return a \code{ggplot2} plot
#' @author Richèl J.C. Bilderbeek
#' @export
check_tree_and_model <- function(tree_and_model) {
  if (length(tree_and_model) != 1) {
    stop("'tree_and_model' must be one value")
  }
  if (!tree_and_model %in% pirouette::get_tree_and_model_values()
  ) {
    stop("'tree_and_model' has invalid value")
  }
}
