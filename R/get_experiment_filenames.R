#' Extract the filenames from an experiment
#' @inheritParams default_params_doc
#' @export
get_experiment_filenames <- function(experiment) {
  check_experiment(experiment)
  c(
    experiment$beast2_options$input_filename,
    experiment$beast2_options$output_log_filename,
    experiment$beast2_options$output_trees_filenames,
    experiment$beast2_options$output_state_filename,
    experiment$errors_filename
  )
}
