#' Extract the filenames from an experiment
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @examples
#' get_experiment_filenames(
#'   experiment = create_test_experiment()
#' )
#' @export
get_experiment_filenames <- function(experiment) {
  pirouette::check_experiment(experiment)

  # Note: can be NA, need alignment ID to initialize
  tracelog_filename <- experiment$inference_model$mcmc$tracelog$filename

  # Note: can be '$(tree).trees', need alignment ID to initialize
  treelog_filename <- experiment$inference_model$mcmc$treelog$filename

  c(
    beastier::get_beast2_options_filenames(experiment$beast2_options),
    tracelog_filename,
    treelog_filename,
    experiment$errors_filename
  )
}
