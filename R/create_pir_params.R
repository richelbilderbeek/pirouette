#' Create the parameters for pirouette
#' @inheritParams default_params_doc
#' @return a list with all pirouette parameters
#' @export
#' @author Giovanni Laudanno
create_pir_params <- function(
  alignment_params,
  twinning_params = NA,
  experiments = list(create_experiment()),
  error_measure_params = create_error_measure_params(),
  evidence_epsilon = 1e-12,
  evidence_filename = tempfile(fileext = ".csv")
) {
  pir_params <- list(
    twinning_params = twinning_params,
    alignment_params = alignment_params,
    experiments = experiments,
    error_measure_params = error_measure_params,
    evidence_epsilon = evidence_epsilon,
    evidence_filename = evidence_filename
  )
  check_pir_params(pir_params) # nolint pirouette function
  pir_params
}
