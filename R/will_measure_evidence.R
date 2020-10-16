#' Determine if there is an experiment in which the evidence will
#' be measured.
#'
#' @inheritParams default_params_doc
#' @examples
#'
#' if (rappdirs::app_dir()$os != "win") {
#'   pir_params <- create_test_pir_params_setup(has_candidate = TRUE)
#'
#'   # Yes, the evidence will be measured on Linux with candidate models
#'   will_measure_evidence(pir_params)
#' }
#'
#' pir_params <- create_test_pir_params_setup(has_candidate = FALSE)
#' # No, the evidence will be measured without candidate models
#' will_measure_evidence(pir_params)
#' @export
will_measure_evidence <- function(pir_params) {
  pirouette::check_pir_params(pir_params)
  for (experiment in pir_params$experiments) {
    if (isTRUE(experiment$inference_conditions$do_measure_evidence)) {
      return(TRUE)
    }
  }
  FALSE
}
