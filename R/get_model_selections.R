#' Get the possible ways to select an inference model
#' @author Richèl J.C. Bilderbeek
#' @examples
#'
#' expect_true("generative" %in% get_model_selections())
#' expect_true("most_evidence" %in% get_model_selections())
#' @export
get_model_selections <- function() {
  c("generative", "most_evidence")
}
