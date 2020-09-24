#' Create a \code{pir_params} that follows a specific setup
#'
#' @param has_candidate TRUE to have a candidate experiment
#' @param has_twinning TRUE to use twinning
#' @examples
#'
#' # Minimal use
#' expect_silent(
#'   check_pir_params(create_test_pir_params_setup())
#' )
#'
#' # Generative experiment only, without twinning
#' pir_params <- create_test_pir_params_setup(
#'   has_candidate = FALSE,
#'   has_twinning = FALSE
#' )
#' !has_twinning(pir_params))
#' !has_candidate_experiments(pir_params))
#'
#' # Generative and candidate experiment, without twinning
#' if (rappdirs::app_dir()$os != "win") {
#'   pir_params <- create_test_pir_params_setup(
#'     has_candidate = TRUE,
#'     has_twinning = FALSE
#'   )
#'   !has_twinning(pir_params))
#'   has_candidate_experiments(pir_params))
#' }
#'
#' # Generative experiment only, with twinning
#' pir_params <- create_test_pir_params_setup(
#'   has_candidate = FALSE,
#'   has_twinning = TRUE
#' )
#' has_twinning(pir_params))
#' !has_candidate_experiments(pir_params))
#'
#' # Generative and candidate experiment, with twinning
#' if (rappdirs::app_dir()$os != "win") {
#'   pir_params <- create_test_pir_params_setup(
#'     has_candidate = TRUE,
#'     has_twinning = TRUE
#'   )
#'   has_twinning(pir_params))
#'   has_candidate_experiments(pir_params))
#' }
#' @export
create_test_pir_params_setup <- function(
  has_candidate = FALSE,
  has_twinning = FALSE
) {
  twinning_params <- NA
  if (isTRUE(has_twinning)) {
    twinning_params <- create_twinning_params()
  }
  experiments <- list(create_test_gen_experiment())


  evidence_filename <- NA
  if (isTRUE(has_candidate)) {
    experiments <- list(
      create_test_gen_experiment(
        inference_model = create_test_inference_model(
          site_model = create_hky_site_model()
        )
      ),
      create_test_cand_experiment()
    )
    evidence_filename <- get_temp_evidence_filename()
    if (isTRUE(has_twinning)) {
      twinning_params$twin_evidence_filename <- get_temp_evidence_filename()
    }
  }
  create_test_pir_params(
    experiments = experiments,
    twinning_params = twinning_params,
    evidence_filename = evidence_filename
  )
}
