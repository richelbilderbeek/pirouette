#' Detect if there is at least one candidate model among the
#' set of experiments.
#' @inheritParams default_params_doc
#' @return one boolean
#' @export
has_candidate_experiments <- function(
  pir_params
) {
  pirouette::check_pir_params(pir_params)
  for (experiment in pir_params$experiments) {
    if (experiment$inference_conditions$model_type == "candidate") {
      return(TRUE)
    }
  }
  FALSE
}
