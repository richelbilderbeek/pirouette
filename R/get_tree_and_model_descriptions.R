#' Internal function to create a mapping from a \code{tree_and_model}
#' to a description
#' @return a \link[tibble]{tibble} with columns \code{tree_and_model}
#'   and \code{description}
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @examples
#'
#' t <- get_tree_and_model_descriptions()
#'
#' expect_silent(check_tree_and_models(t$tree_and_model))
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
