#' Create the parameters for \link{pirouette}
#' @inheritParams default_params_doc
#' @return a list with all \link{pirouette} parameters
#' @seealso
#'   \itemize{
#'     \item Use \link{pir_run} to run the \link{pirouette} pipeline
#'     \item Use \link{create_test_pir_params} to create
#'       a test \code{pir_params}
#'     \item Use \link{create_test_pir_params_setup} to create
#'       a test \code{pir_params} following a specific setup, such as
#'       having a candidate experiemnt and/or use twinning.
#'   }
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#'
#' # Create all elements to create a pirouette parameter set
#' alignment_params <- create_test_alignment_params()
#' twinning_params <- create_twinning_params()
#' experiments <- list(create_test_gen_experiment())
#' error_measure_params <- create_error_measure_params()
#' evidence_filename <- NA
#' verbose <- FALSE
#'
#' # Create the pirouette parameter set
#' pir_params <- create_pir_params(
#'   alignment_params = alignment_params,
#'   twinning_params = twinning_params,
#'   experiments = experiments,
#'   error_measure_params = error_measure_params,
#'   evidence_filename = evidence_filename,
#'   verbose = verbose
#' )
#'
#' expect_equal(alignment_params, pir_params$alignment_params)
#' expect_equal(twinning_params, pir_params$twinning_params)
#' expect_equal(experiments, pir_params$experiments)
#' expect_equal(error_measure_params, pir_params$error_measure_params)
#' expect_equal(evidence_filename, pir_params$evidence_filename)
#' expect_equal(verbose, pir_params$verbose)
#'
#' # Run that experiment on a continuous integration service,
#' # only when BEAST2 is unstalled
#' if (rappdirs::app_dir()$os != "win" &&
#'   is_on_ci() && is_beast2_installed()
#' ) {
#'   pir_out <- pir_run(
#'     phylogeny = ape::read.tree(text = "((A:2, B:2):1, C:3);"),
#'     pir_params = pir_params
#'   )
#'   pir_plot(pir_out)
#' }
#' @export
create_pir_params <- function(
  alignment_params,
  twinning_params = NA,
  experiments = list(create_experiment()),
  error_measure_params = create_error_measure_params(),
  evidence_filename = NA,
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
  pirouette::check_pir_params(pir_params)
  pir_params
}

#' Create a set of testing parameters for \link{pirouette}
#' @inheritParams default_params_doc
#' @return a list with all \link{pirouette} parameters
#' @seealso Use \link{create_test_pir_params_setup} to create
#' a test \code{pir_params} following a specific setup, such as
#' having a candidate experiemnt and/or use twinning.
#' @examples
#' if (rappdirs::app_dir()$os != "win" &&
#'   is_on_ci() &&
#'   is_beast2_installed()
#' ) {
#'   pir_params <- create_test_pir_params()
#'   check_pir_params(pir_params)
#'
#'   pir_out <- pir_run(
#'     phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
#'     pir_params = pir_params
#'   )
#'   pir_plot(pir_out)
#' }
#' @export
#' @author Richèl J.C. Bilderbeek
create_test_pir_params <- function(
  alignment_params = create_test_alignment_params(),
  twinning_params = NA,
  experiments = list(create_test_experiment()),
  error_measure_params = create_error_measure_params(),
  evidence_filename = NA,
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
