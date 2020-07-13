#' Get the valid \code{tree_and_model} values
#' @return the four valid \code{tree_and_model} values
#' @author Richèl J.C. Bilderbeek
#' @export
get_tree_and_model_values <- function() {
  c(
    "true_generative", "twin_generative", "true_candidate", "twin_candidate"
  )
}
