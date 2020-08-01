#' Internal function
#'
#' Internal function to relevel the \code{inference_model}, so that
#' \link{pir_plot} has the legend labels in the right order
#' @inheritParams default_params_doc
#' @return a releveled \link{inference_model}
#' @author Richèl J.C. Bilderbeek
#' @export
relevel_inference_model <- function(inference_model) {
  # This is the right order
  target_levels <- c(
    "generative",
    "candidate"
  )
  # Remove levels that are not present
  target_levels <- target_levels[target_levels %in% inference_model]

  # 'generative' must be the first level for the facet plot
  inference_model <- forcats::fct_relevel(
    inference_model,
    target_levels
  )
  inference_model
}
