#' Determine if there is an experiment in which the evidence will
#' be measured.
#'
#' @inheritParams default_params_doc
#' @examples
#' library(testthat)
#'
#' pir_params <- create_test_pir_params_setup(has_candidate = TRUE)
#' expect_true(will_measure_evidence(pir_params))
#'
#' pir_params <- create_test_pir_params_setup(has_candidate = FALSE)
#' expect_true(!will_measure_evidence(pir_params))
#'
#' pir_params$experiments[[1]]$inference_conditions$do_measure_evidence <- TRUE
#' expect_true(will_measure_evidence(pir_params))
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