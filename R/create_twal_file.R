#' Simulates a twin DNA alignment and saves it to a FASTA file.
#'
#' The simulation is performed by \link{sim_twin_alignment}.
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{sim_twin_alignment} to only simulate
#'   the twin alignment, without saving the alignment to file
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' # Create a true phylogeny to simulate the DNA sequences on
#' n_taxa <- 5
#' set.seed(1); phylogeny <- ape::rcoal(n_taxa)
#'
#' # Create the ancestor's DNA sequence
#' n_base_pairs <- 4
#' root_sequence <- create_blocked_dna(length = n_base_pairs)
#'
#' # Simulate and save the true alignment
#' alignment_params <- create_alignment_params(
#'   root_sequence = root_sequence
#' )
#' create_tral_file(
#'   phylogeny = phylogeny,
#'   alignment_params = alignment_params
#' )
#'
#' # Create a twin phylogeny to simulate the DNA sequences on
#' set.seed(2); twin_phylogeny <- ape::rcoal(n_taxa)
#'
#' # File does not exist yet
#' twinning_params <- create_twinning_params()
#' expect_true(!file.exists(twinning_params$twin_alignment_filename))
#'
#' # Simulate and save the twin alignment
#' alignment <- create_twal_file(
#'  twin_phylogeny = twin_phylogeny,
#'  alignment_params = alignment_params,
#'  twinning_params = twinning_params
#' )
#' # File does exist now
#' expect_true(file.exists(twinning_params$twin_alignment_filename))
#' @export
create_twal_file <- function(
  twin_phylogeny,
  alignment_params,
  twinning_params
) {
  pirouette::check_twin_phylogeny(twin_phylogeny)
  pirouette::check_alignment_params(alignment_params)
  pirouette::check_twinning_params(twinning_params)

  true_alignment_filename <- alignment_params$fasta_filename
  true_alignment <- ape::read.FASTA(true_alignment_filename)

  # Simulate
  twin_alignment <- pirouette::sim_twin_alignment(
    twin_phylogeny = twin_phylogeny,
    true_alignment = true_alignment,
    alignment_params = alignment_params,
    twinning_params = twinning_params
  )

  # Save
  twin_alignment_filename <- twinning_params$twin_alignment_filename
  # Create a sub-sub-sub folder, don't warn when it already exists
  dir.create(dirname(
    twin_alignment_filename),
    showWarnings = FALSE, recursive = TRUE
  )

  phangorn::write.phyDat(
    twin_alignment,
    file = twin_alignment_filename,
    format = "fasta"
  )
  beautier::check_file_exists(
    twin_alignment_filename,
    "twin_alignment_filename"
  )
}
