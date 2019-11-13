#' Converts a phylogeny to a random DNA alignment
#'
#' The function is used to create both
#' the true (see \link{create_true_alignment})
#' and twin alignment (see \link{sim_twin_alignment}).
#' @inheritParams default_params_doc
#' @param n_mutations the number of different base pairs between
#' root sequence and the resulting alignment. Set to \link{NA} if
#' any number of mutations is fine.
#' @param max_n_tries number of attempts to simulate a DNA alignment
#' with the desired number of mutations. If this number of attempts is
#' reached, the funcion will show a \link{warning} and return
#' the last DNA alignment simulated.
#' @return an alignment of type \code{DNAbin}
#' @seealso Use \link{create_alignment_file} to save the simulated alignment
#'   directly to a file
#' @examples
#' library(testthat)
#'
#' # Create the phylogeny to simulate the alignment on
#' n_taxa <- 5
#' phylogeny <- ape::rcoal(n_taxa)
#'
#' # Use default settings to create the alignment
#' alignment_params <- pirouette::create_alignment_params()
#'
#' # Simulate the alignment
#' alignment <- sim_true_alignment(
#'    true_phylogeny = phylogeny,
#'    alignment_params = alignment_params,
#'  )
#'
#' expect_silent(check_alignment(alignment))
#' expect_equal(nrow(alignment), n_taxa)
#' expect_equal(ncol(alignment), nchar(alignment_params$root_sequence))
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @seealso Use \link{sim_true_alignment_with_standard_site_model}
#' simulate the true alignment with a standard site model.
#' Use \link{sim_twin_alignment_with_standard_site_model}
#' simulate the twin alignment with a standard site model.
#' @export
sim_alignment_with_n_mutations <- function(
  phylogeny,
  root_sequence,
  n_mutations,
  mutation_rate = 1.0,
  site_model = beautier::create_jc69_site_model(),
  max_n_tries = 100,
  verbose = FALSE
) {
  beautier::check_phylogeny(phylogeny)
  pirouette::check_reconstructed_phylogeny(phylogeny)
  pirouette::check_root_sequence(root_sequence)
  pirouette::check_mutation_rate(mutation_rate)
  testit::assert(beautier::is_one_int(n_mutations))
  testit::assert(n_mutations >= 0)
  testit::assert(beautier::is_one_bool(verbose))

  # Higher-level checks
  n_taxa <- ape::Ntip(phylogeny)
  n_nucleotides <- nchar(root_sequence)
  max_n_mutations <- n_taxa * n_nucleotides
  if (!beautier::is_one_na(n_mutations) && n_mutations > max_n_mutations) {
    stop(
      "Cannot have more mutations than the total number of nucleotides in the ",
      "alignment. \n",
      "Number of taxa: ", n_taxa, "\n",
      "Number of nucleotides per taxon: ", n_nucleotides, "\n",
      "Maximum number of mutations: ", max_n_mutations, "\n",
      "Requested number of mutations: ", n_mutations
    )
  }
  n_tries <- 1

  while (n_tries < max_n_tries) {

    alignment <- sim_true_alignment_with_standard_site_model(
      true_phylogeny = phylogeny,
      root_sequence = root_sequence,
      mutation_rate = mutation_rate,
      site_model = site_model
    )
    pirouette::check_alignment(alignment)

    actual_n_mutations <- count_n_mutations(
      alignment = alignment,
      root_sequence = root_sequence
    )

    if (verbose == TRUE) {
      print(
        paste0(
          "Mutations needed: ", n_mutations,
          ", got: ", actual_n_mutations,
          ", number of tries: ", n_tries
        )
      )
    }

    if (actual_n_mutations == n_mutations) break

    n_tries <- n_tries + 1

    if (n_tries == max_n_tries) {
      warning(
        "'sim_alignment_with_n_mutations' tried ", n_tries, " times, ",
        "without simulating an alignment with ", n_mutations, " mutations"
      )
    }
  }

  pirouette::check_alignment(alignment)
  testit::assert(
    get_alignment_sequence_length(alignment) ==
    nchar(root_sequence)
  )
  testit::assert(
    get_alignment_n_taxa(alignment) ==
    ape::Ntip(phylogeny)
  )
  alignment
}
