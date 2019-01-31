#' Checks if the argument is a valid \link{pirouette} experiment.
#'
#' Will \link{stop} if not.
#' A valid \link{pirouette} experiment
#' can be created by \link{create_experiment}.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if not
#' @author Richel J.C. Bilderbeek
check_experiment <- function(
  experiment
) {
  return()

  argument_names <- c(
    "rng_seed", "twin_model", "twin_tree_filename", "twin_alignment_filename"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(experiment)) {
      stop(
        "'", arg_name, "' must be an element of an 'experiment'. ",
        "Tip: use 'create_experiment'"
      )
    }
  }
  if (!is.numeric(experiment$rng_seed)) {
    stop("'rng_seed' must be a number")
  }
  if (!is.character(experiment$twin_model)) {
    stop("'twin_model' must be a character vector")
  }
  if (!(experiment$twin_model %in% get_twin_models())) {
   stop("This twin model is not implemented")
  }
  if (!is.character(experiment$twin_tree_filename)) {
    stop("'twin_tree_filename' must be a character vector")
  }
  if (!is.character(experiment$twin_alignment_filename)) {
    stop("'twin_alignment_filename' must be a character vector")
  }

}
