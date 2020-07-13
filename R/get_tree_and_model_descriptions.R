#' Internal function to creat a mapping from a \code{tree_and_model}
#' @return a \link[tibble]{tibble} with columns \code{tree_and_model}
#'   and \code{description}
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @export
get_tree_and_model_descriptions <- function() {# nolint long function name is fine for an internal function
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
