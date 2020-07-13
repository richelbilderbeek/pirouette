#' Internal function to creat a mapping from a \link{tree_and_model}
#' @return a \link[tibble]{tibble} with columns \link{tree_and_model}
#'   and \link{description}
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @export
get_tree_and_model_descriptions <- function() {
  t <- tibble::tribble(
    ~tree_and_model, ~description,
    "true_generative", "Generative, true",
    "twin_generative", "Generative, twin",
    "true_candidate", "Best, true",
    "twin_candidate", "Best, twin"
  )
  t$tree_and_model <- as.factor(t$tree_and_model)
  t
}
