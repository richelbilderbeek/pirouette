#' Converts a phylogeny to a random DNA alignment
#'
#' The function is used to create both
#' the true (see \link{create_true_alignment})
#' and twin alignment (see \link{sim_twin_alignment}).
#' @inheritParams default_params_doc
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
#' alignment <- create_alignment(
#'    phylogeny = phylogeny,
#'    alignment_params = alignment_params,
#'  )
#'
#' expect_equal(class(alignment), "DNAbin")
#' expect_equal(nrow(alignment), n_taxa)
#' expect_equal(ncol(alignment), nchar(alignment_params$root_sequence))
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @seealso Use \link{sim_alignment_with_n_mutations} to
#' simulate an alignmnet with a certain number of mutations
#' @export
create_alignment <- function(
  phylogeny,
  alignment_params,
  verbose = FALSE
) {
  beautier::check_phylogeny(phylogeny)
  pirouette::check_alignment_params(alignment_params)
  pirouette::check_reconstructed_phylogeny(phylogeny)
  testit::assert(beautier::is_one_bool(verbose))
  n_taxa <- ape::Ntip(phylogeny)
  n_nucleotides <- nchar(alignment_params$root_sequence)

  # If mutation_rate is function, apply it to the phylogeny
  if (is.function(alignment_params$mutation_rate)) {
    mutation_function <- alignment_params$mutation_rate
    alignment_params$mutation_rate <- mutation_function(phylogeny)
  }
  testit::assert(alignment_params$mutation_rate >= 0.0)

  # Only need to set the seed once
  set.seed(alignment_params$rng_seed)

  # Do it
  alignment <- NA
  if (beautier::is_site_model(alignment_params$site_model)) {
    # Standard site models
    alignment <- pirouette::sim_alignment_with_std_site_model(
      phylogeny = phylogeny,
      root_sequence = alignment_params$root_sequence,
      mutation_rate = alignment_params$mutation_rate,
      site_model = alignment_params$site_model
    )
    # alignment <- create_alignment_with_standard_site_model(
    #   phylogeny = phylogeny,
    #   alignment_params = alignment_params
    # )
  } else if (alignment_params$site_model == "linked_node_sub") {
    alignment <- sim_true_alignment_with_linked_node_sub_site_model(
      true_phylogeny = phylogeny,
      root_sequence = alignment_params$root_sequence
    )
  } else {
    testit::assert(alignment_params$site_model == "unlinked_node_sub")
    alignment <- sim_true_alignment_with_unlinked_node_sub_site_model(
      true_phylogeny = phylogeny,
      root_sequence = alignment_params$root_sequence
    )
  }
  pirouette::check_alignment(alignment)

  testit::assert(
    get_alignment_sequence_length(alignment) ==
    nchar(alignment_params$root_sequence)
  )
  testit::assert(
    get_alignment_n_taxa(alignment) ==
    ape::Ntip(phylogeny)
  )
  alignment
}
