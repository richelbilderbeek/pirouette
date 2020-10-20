#' Create a \code{pir_params} that follows a specific setup
#'
#' @param has_candidate TRUE to have a candidate experiment
#' @param has_twinning TRUE to use twinning
#' @examples
#' # Minimal use
#' check_pir_params(create_test_pir_params_setup())
#'
#' # Generative experiment only, without twinning
#' create_test_pir_params_setup(
#'   has_candidate = FALSE,
#'   has_twinning = FALSE
#' )
#'
#' # Generative and candidate experiment, without twinning
#' if (rappdirs::app_dir()$os != "win") {
#'   create_test_pir_params_setup(
#'     has_candidate = TRUE,
#'     has_twinning = FALSE
#'   )
#' }
#'
#' # Generative experiment only, with twinning
#' create_test_pir_params_setup(
#'   has_candidate = FALSE,
#'   has_twinning = TRUE
#' )
#'
#' # Generative and candidate experiment, with twinning
#' if (rappdirs::app_dir()$os != "win") {
#'   create_test_pir_params_setup(
#'     has_candidate = TRUE,
#'     has_twinning = TRUE
#'   )
#' }
#' @export
create_test_pir_params_setup <- function(
  has_candidate = FALSE,
  has_twinning = FALSE
) {
  twinning_params <- NA
  if (isTRUE(has_twinning)) {
    twinning_params <- pirouette::create_twinning_params()
  }
  experiments <- list(pirouette::create_test_gen_experiment())


  evidence_filename <- NA
  if (isTRUE(has_candidate)) {
    experiments <- list(
      pirouette::create_test_gen_experiment(
        inference_model = beautier::create_test_inference_model(
          site_model = beautier::create_hky_site_model()
        )
      ),
      create_test_cand_experiment()
    )
    evidence_filename <- pirouette::get_temp_evidence_filename()
    if (isTRUE(has_twinning)) {
      twinning_params$twin_evidence_filename <-
        pirouette::get_temp_evidence_filename()
    }
  }
  pirouette::create_test_pir_params(
    experiments = experiments,
    twinning_params = twinning_params,
    evidence_filename = evidence_filename
  )
}
