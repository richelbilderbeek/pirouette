#' Create the parameters for \code{pirouette}
#' @inheritParams default_params_doc
#' @return a list with all \code{pirouette} parameters
#' @export
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
create_pir_params <- function(
  alignment_params,
  twinning_params = NA,
  experiments = list(create_experiment()),
  error_measure_params = create_error_measure_params(),
  evidence_filename = tempfile(pattern = "evidence_", fileext = ".csv"),
  verbose = FALSE
) {
  pir_params <- list(
    twinning_params = twinning_params,
    alignment_params = alignment_params,
    experiments = experiments,
    error_measure_params = error_measure_params,
    evidence_filename = evidence_filename,
    verbose = verbose
  )
  check_pir_params(pir_params) # nolint pirouette function
  pir_params
}

#' Create a set of testing parameters for \code{pirouette}
#' @inheritParams default_params_doc
#' @return a list with all \code{pirouette} parameters
#' @examples
#'   pir_params <- create_test_pir_params()
#'   check_pir_params(pir_params)
#' @export
#' @author Richel J.C. Bilderbeek
create_test_pir_params <- function(
  alignment_params = create_test_alignment_params(),
  twinning_params = NA,
  experiments = list(create_test_experiment()),
  error_measure_params = create_error_measure_params(),
  evidence_filename = tempfile(pattern = "evidence_", fileext = ".csv"),
  verbose = FALSE
) {
  create_pir_params(
    alignment_params = alignment_params,
    twinning_params = twinning_params,
    experiments = experiments,
    error_measure_params = error_measure_params,
    evidence_filename = evidence_filename,
    verbose = verbose
  )
}
