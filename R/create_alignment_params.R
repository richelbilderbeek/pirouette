#' Create the parameters for the alignment simulation
#' @inheritParams default_params_doc
#' @param rng_seed the random number generator seed as used in the
#'   simulation of an alignment
#' @return a list of alignment parameters
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
