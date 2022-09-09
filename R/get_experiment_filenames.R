#' Extract the filenames from an experiment
#' @inheritParams default_params_doc
#' @return a character vector
#' @author Richèl J.C. Bilderbeek
#' @examples
#' if (beautier::is_on_ci()) {
#'   get_experiment_filenames(
#'     experiment = create_test_experiment()
#'   )
#' }
#' @export
get_experiment_filenames <- function(experiment) {
  pirouette::check_experiment(experiment)

  stats::na.omit(
    c(
      beastier::get_beast2_options_filenames(experiment$beast2_options),
      # Note: can be NA, need alignment ID to initialize
      experiment$inference_model$mcmc$tracelog$filename,
      experiment$inference_model$mcmc$screenlog$filename,
      # Note: can be '$(tree).trees', need alignment ID to initialize
      experiment$inference_model$mcmc$treelog$filename,
      experiment$est_evidence_mcmc$tracelog$filename,
      experiment$est_evidence_mcmc$screenlog$filename,
      experiment$est_evidence_mcmc$treelog$filename,
      experiment$errors_filename
    )
  )
}
