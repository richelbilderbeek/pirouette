#' Get the possible ways to select an inference model
#' @return a character vector
#' @author Richèl J.C. Bilderbeek
#' @examples
#' get_model_selections()
#' @export
get_model_selections <- function() {
  c("generative", "most_evidence")
}
