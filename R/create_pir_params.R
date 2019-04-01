#' Create the parameters for \link{pirouette}
#' @inheritParams default_params_doc
#' @return a list with all \link{pirouette} parameters
#' @seealso Use \link{pir_run} to run the \link{pirouette} pipeline
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @examples
#'   alignment_params <- create_test_alignment_params()
#'   twinning_params <- create_twinning_params()
#'   experiments <- list(create_test_experiment())
#'   error_measure_params <- create_error_measure_params()
#'   evidence_filename <- tempfile(fileext = ".csv")
#'   verbose <- FALSE
#'
#'   pir_params <- create_pir_params(
#'     alignment_params = alignment_params,
#'     twinning_params = twinning_params,
#'     experiments = experiments,
#'     error_measure_params = error_measure_params,
#'     evidence_filename = evidence_filename,
#'     verbose = verbose
#'   )
#'
#'   library(testthat)
#'   expect_equal(alignment_params, pir_params$alignment_params)
#'   expect_equal(twinning_params, pir_params$twinning_params)
#'   expect_equal(experiments, pir_params$experiments)
#'   expect_equal(error_measure_params, pir_params$error_measure_params)
#'   expect_equal(evidence_filename, pir_params$evidence_filename)
#'   expect_equal(verbose, pir_params$verbose)
#' @export
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

#' Create a set of testing parameters for \link{pirouette}
#' @inheritParams default_params_doc
#' @return a list with all \link{pirouette} parameters
#' @examples
#'   pir_params <- create_test_pir_params()
#'   check_pir_params(pir_params)
#'
#'   if (is_on_ci() && is_beast2_installed()) {
#'     pir_out <- pir_run(
#'       phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
#'       pir_params = pir_params
#'     )
#'
#'     pir_plot(pir_run)
#'   }
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
