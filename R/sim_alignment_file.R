#' Simulates a DNA alignment and saves it to a FASTA file.
#'
#' The simulation is performed by \link{sim_alignment}.
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{sim_alignment} to only simulate the alignment,
#'   without saving the alignment to file
#' @examples
#' library(testthat)
#'
#' # Create the ancestor's DNA sequence
#' n_base_pairs <- 4
#' root_sequence <- create_blocked_dna(length = n_base_pairs)
#'
#' # How to simulate (and where to save) the alignment
#' alignment_params <- create_alignment_params(
#'   root_sequence = root_sequence,
#'   mutation_rate = 0.1
#' )
#'
#' # Create a phylogeny to simulate the DNA sequences on
#' n_taxa <- 5
#' phylogeny <- ape::rcoal(n_taxa)
#'
#' # File does not exist yet
#' expect_false(file.exists(alignment_params$fasta_filename))
#'
#' # Simulate and save the alignment
#' alignment <- sim_alignment_file(
#'   phylogeny = phylogeny,
#'   alignment_params = alignment_params
#' )
#' # File does exist now
#' expect_true(file.exists(alignment_params$fasta_filename))
#' @author Richèl J.C. Bilderbeek
#' @export
sim_alignment_file <- function(
  phylogeny,
  alignment_params
) {
  fasta_filename <- alignment_params$fasta_filename

  # Simulate
  alignment <- sim_alignment(
    phylogeny = phylogeny,
    alignment_params = alignment_params
  )
  # Save
  phangorn::write.phyDat(
    alignment,
    file = fasta_filename,
    format = "fasta"
  )
}
