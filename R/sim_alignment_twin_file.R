#' Simulates a twin DNA alignment and saves it to a FASTA file.
#'
#' The simulation is performed by \link{sim_alignment_twin}.
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{sim_alignment_twin} to only simulate the twin alignment,
#'   without saving the alignment to file
#' @author Richèl J.C. Bilderbeek
#' @export
sim_alignment_twin_file <- function(
  twin_phylogeny,
  alignment_params,
  twinning_params
) {
  check_twin_phylogeny(twin_phylogeny) # nolint pirouette function
  check_alignment_params(alignment_params) # nolint pirouette function
  check_twinning_params(twinning_params) # nolint pirouette function

  true_alignment_filename <- alignment_params$fasta_filename
  true_alignment <- ape::read.FASTA(true_alignment_filename)
  n_mutations_true <- count_n_mutations(
    alignment = true_alignment,
    root_sequence = alignment_params$root_sequence
  )

  # Simulate
  twin_alignment <- sim_alignment_twin(
    twin_phylogeny = twin_phylogeny,
    root_sequence = alignment_params$root_sequence,
    rng_seed_twin_alignment = twinning_params$rng_seed_twin_alignment,
    mutation_rate = alignment_params$mutation_rate,
    site_model = alignment_params$site_model,
    n_mutations = n_mutations_true
  )

  n_mutations_twin <- count_n_mutations(
    alignment = twin_alignment,
    root_sequence = alignment_params$root_sequence
  )
  testit::assert(n_mutations_true == n_mutations_twin)

  # Save
  phangorn::write.phyDat(
    twin_alignment,
    file = twinning_params$twin_alignment_filename,
    format = "fasta"
  )
}
