#' Checks if the argument is a valid alignment parameters structure.
#' Will \link{stop} if not
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if nit
#' @author Richel J.C. Bilderbeek
check_alignment_params <- function(
  alignment_params
) {
  if (!"root_sequence" %in% names(alignment_params)) {
    stop(
      "'root_sequence' must be an element of an 'alignment_params'. ",
      "Tip: use 'create_alignment_params'"
    )
  }
  if (!"mutation_rate" %in% names(alignment_params)) {
    stop(
      "'mutation_rate' must be an element of an 'alignment_params'. ",
      "Tip: use 'create_alignment_params'"
    )
  }

  if (!is_dna_seq(alignment_params$root_sequence)) {
    stop("'root_sequence' must be a lowercase DNA character string")
  }
  if (alignment_params$mutation_rate < 0) {
    stop("'mutation_rate' must be a non-zero and positive value")
  }
}
