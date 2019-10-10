#' Check that the \code{twinning_params} has all the list elements
#' @export
check_twinning_params_list_elements <- function(twinning_params) { # nolint indeed long function name, which is fine for an internal function
  argument_names <- c(
    "rng_seed_twin_tree",
    "rng_seed_twin_alignment",
    "twin_model",
    "method",
    "n_replicates",
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
}
