#' Internal function
#'
#' Internal function to relevel the \code{tree_and_model}, so that
#' \link{pir_plot} has the legend labels in the right order
#' @inheritParams default_params_doc
#' @return a releveled \link{tree_and_model}
#' @author Richèl J.C. Bilderbeek
#' @export
relevel_tree_and_model <- function(tree_and_model) {
  # This is the right order
  target_levels <- c(
    "true_generative",
    "twin_generative",
    "true_candidate",
    "twin_candidate"
  )
  # Remove levels that are not present
  target_levels <- target_levels[target_levels %in% tree_and_model]

  # 'generative' must be the first level for the facet plot
  tree_and_model <- forcats::fct_relevel(
    tree_and_model,
    target_levels
  )
  tree_and_model
}
