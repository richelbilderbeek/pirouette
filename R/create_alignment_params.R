#' Create the parameters for the alignment simulation.
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
  root_sequence = pirouette::create_blocked_dna(1000),
  mutation_rate = 0.0,
  site_model = beautier::create_jc69_site_model(),
  clock_model = beautier::create_strict_clock_model(),
  rng_seed = 0,
  fasta_filename = tempfile(fileext = ".fasta")
) {
  alignment_params <- list(
    root_sequence = root_sequence,
    mutation_rate = mutation_rate,
    site_model = site_model,
    clock_model = clock_model,
    rng_seed = rng_seed,
    fasta_filename = fasta_filename
  )
  check_alignment_params(alignment_params = alignment_params) # nolint pirouette function
  alignment_params
}

#' Create testing parameters for the alignment simulation.
#' @inheritParams default_params_doc
#' @return a list of alignment parameters
#' @export
#' @author Richel J.C. Bilderbeek
create_test_alignment_params <- function(
) {
  create_alignment_params(root_sequence = "acgt", mutation_rate = 0.1)
}
