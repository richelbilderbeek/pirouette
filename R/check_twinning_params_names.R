#' Check that the \code{twinning_params} has all the list elements' names
#' @inheritParams default_params_doc
#' @return nothing
#' @author Richèl J.C. Bilderbeek
#' @export
check_twinning_params_names <- function(twinning_params) { # nolint indeed long function name, which is fine for an internal function
  argument_names <- c(
    "rng_seed_twin_tree",
    "sim_twin_tree_fun",
    "rng_seed_twin_alignment",
    "sim_twal_fun",
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
  invisible(twinning_params)
}
