#' Checks if the argument is a valid alignment parameters structure,
#' as created by \link{create_alignment_params}.
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if nit
#' @author Richel J.C. Bilderbeek
check_alignment_params <- function(
  alignment_params
) {
  argument_names <- c(
    "root_sequence", "mutation_rate", "rng_seed",
    "site_model", "clock_model"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(alignment_params)) {
      stop(
        "'", arg_name, "' must be an element of an 'alignment_params'. ",
        "Tip: use 'create_alignment_params'"
      )
    }
  }
  if (!is_dna_seq(alignment_params$root_sequence)) {
    stop("'root_sequence' must be a lowercase DNA character string")
  }
  if (alignment_params$mutation_rate < 0) {
    stop("'mutation_rate' must be a non-zero and positive value")
  }
  if (!is.numeric(alignment_params$rng_seed)) {
    stop("'rng_seed' must be a number")
  }
}
