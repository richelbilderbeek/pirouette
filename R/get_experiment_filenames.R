#' Extract the filenames from an experiment
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @examples
#' get_experiment_filenames(
#'   experiment = create_test_experiment()
#' )
#' @export
get_experiment_filenames <- function(experiment) {
  check_experiment(experiment) # nolint pirouette function
  c(
    get_beast2_options_filenames(experiment$beast2_options),
    experiment$errors_filename
  )
}
