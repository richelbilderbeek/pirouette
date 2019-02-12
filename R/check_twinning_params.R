#' Checks if the argument is a valid twinning parameters structure,
#' as created by \link{create_twinning_params}.
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if nit
#' @author Richel J.C. Bilderbeek
check_twinning_params <- function(
  twinning_params
) {
  argument_names <- c(
    "rng_seed",
    "twin_model",
    "method",
    "n_replicas",
    "twin_tree_filename",
    "twin_alignment_filename",
    "twin_evidence_filename"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(twinning_params)) {
      stop(
        "'", arg_name, "' must be an element of an 'twinning_params'. ",
        "Tip: use 'create_twinning_params'"
      )
    }
  }
  if (!(twinning_params$rng_seed == "same_seed")) {
    if (!is.numeric(twinning_params$rng_seed)) {
      stop("'rng_seed' must be a number or 'same_seed'")
    }
  }
  if (!is.character(twinning_params$twin_model)) {
    stop("'twin_model' must be a character vector")
  }
  if (!(twinning_params$twin_model %in% get_twin_models())) {
   stop("This 'twin model' is not implemented")
  }
  if (!is.character(twinning_params$method)) {
    stop("'method' must be a character vector")
  }
  if (!(twinning_params$method %in% get_twin_methods())) {
    stop("This 'method' is not implemented")
  }
  if (!is.numeric(twinning_params$n_replicas)) {
    stop("'n_replicas' must be a number")
  }
  if (
    is.infinite(twinning_params$n_replicas) |
    !(twinning_params$n_replicas %% 1 == 0) |
    twinning_params$n_replicas < 0
  ) {
    stop("'n_replicas' must be a finite positive integer number")
  }
  if (!is.character(twinning_params$twin_tree_filename)) {
    stop("'twin_tree_filename' must be a character vector")
  }
  if (!is.character(twinning_params$twin_alignment_filename)) {
    stop("'twin_alignment_filename' must be a character vector")
  }
  if (!is.character(twinning_params$twin_evidence_filename)) {
    stop("'twin_evidence_filename' must be a character vector")
  }

}
