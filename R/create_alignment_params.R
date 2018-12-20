#' Create the parameters for the alignment simulation
#' @inheritParams default_params_doc
#' @param rng_seed the random number generator seed as used in the
#'   simulation of an alignment
#' @return a list of alignment parameters
#' @examples
#' n_base_pairs <- 4
#' alignment_params <- create_alignment_params(
#'    root_sequence = create_blocked_dna(length = n_base_pairs),
#'    mutation_rate = 0.1
#'  )
#'  testit::assert("root_sequence" %in% names(alignment_params))
#'  testit::assert("mutation_rate" %in% names(alignment_params))
#'  testit::assert("rng_seed" %in% names(alignment_params))
#' @export
#' @author Richel J.C. Bilderbeek
create_alignment_params <- function(
  root_sequence,
  mutation_rate,
  rng_seed = 0
) {
  list(
    root_sequence = root_sequence,
    mutation_rate = mutation_rate,
    rng_seed = rng_seed
  )
}
