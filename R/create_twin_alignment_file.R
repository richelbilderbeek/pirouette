#' Simulates a twin DNA alignment and saves it to a FASTA file.
#'
#' The simulation is performed by \link{create_twin_alignment}.
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_twin_alignment} to only simulate
#'   the twin alignment, without saving the alignment to file
#' @author Richèl J.C. Bilderbeek
#' @export
create_twin_alignment_file <- function(
  twin_phylogeny,
  alignment_params,
  twinning_params,
  verbose = FALSE
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
  twin_alignment <- create_twin_alignment(
    twin_phylogeny = twin_phylogeny,
    true_alignment = true_alignment,
    alignment_params = alignment_params,
    twinning_params = twinning_params,
    verbose = verbose
  )

  n_mutations_twin <- count_n_mutations(
    alignment = twin_alignment,
    root_sequence = alignment_params$root_sequence
  )
  testit::assert(n_mutations_true == n_mutations_twin)

  # Save
  twin_alignment_filename <- twinning_params$twin_alignment_filename
  if (isTRUE(verbose)) {
    print(paste0("Saving twin alignment to '", twin_alignment_filename, "'"))
  }

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
