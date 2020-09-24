#' Simulates a DNA alignment and saves it to a FASTA file.
#'
#' The simulation is performed by \link{create_true_alignment}.
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_true_alignment} to only simulate the alignment,
#'   without saving the alignment to file
#' @examples
#'
#' # Create the ancestor's DNA sequence
#' n_base_pairs <- 4
#' root_sequence <- create_blocked_dna(length = n_base_pairs)
#'
#' # How to simulate (and where to save) the alignment
#' alignment_params <- create_alignment_params(
#'   root_sequence = root_sequence
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
#' alignment <- create_tral_file(
#'   phylogeny = phylogeny,
#'   alignment_params = alignment_params
#' )
#' # File does exist now
#' expect_true(file.exists(alignment_params$fasta_filename))
#' @author Richèl J.C. Bilderbeek
#' @export
create_tral_file <- function(
  phylogeny,
  alignment_params,
  verbose = FALSE
) {
  fasta_filename <- alignment_params$fasta_filename

  # Simulate
  alignment <- create_true_alignment(
    true_phylogeny = phylogeny,
    alignment_params = alignment_params
  )
  # Save
  # Make sure the (sub-sub-) folder exists for the file,
  # but do not warn if it already exists
  if (isTRUE(verbose)) {
    message("Saving alignment to '", fasta_filename, "'")
  }
  dir.create(
    path = dirname(fasta_filename),
    showWarnings = FALSE,
    recursive = TRUE
  )
  phangorn::write.phyDat(
    alignment,
    file = fasta_filename,
    format = "fasta"
  )
  beautier::check_file_exists(fasta_filename, "fasta_filename")
}
