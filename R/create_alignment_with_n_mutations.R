#' Converts a phylogeny to a random DNA alignment
#'
#' The function is used to create both
#' the true (see \link{create_true_alignment})
#' and twin alignment (see \link{create_twin_alignment}).
#' @inheritParams default_params_doc
#' @param n_mutations the number of different base pairs between
#' root sequence and the resulting alignment. Set to \link{NA} if
#' any number of mutations is fine.
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
#' @export
create_alignment_with_n_mutations <- function(
  phylogeny,
  alignment_params,
  n_mutations,
  verbose = FALSE
) {
  beautier::check_phylogeny(phylogeny)
  pirouette::check_alignment_params(alignment_params)
  pirouette::check_reconstructed_phylogeny(phylogeny)
  testit::assert(beautier::is_one_int(n_mutations))
  testit::assert(n_mutations >= 0)
  testit::assert(beautier::is_one_bool(verbose))
  n_taxa <- ape::Ntip(phylogeny)
  n_nucleotides <- nchar(alignment_params$root_sequence)
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

  # If mutation_rate is function, apply it to the phylogeny
  if (is.function(alignment_params$mutation_rate)) {
    mutation_function <- alignment_params$mutation_rate
    alignment_params$mutation_rate <- mutation_function(phylogeny)
  }
  testit::assert(alignment_params$mutation_rate >= 0.0)
  if (!beautier::is_one_na(n_mutations) && n_mutations > 0) {
    testit::assert(alignment_params$mutation_rate > 0.0)
  }

  if (!beautier::is_one_int(n_mutations) && !beautier::is_one_na(n_mutations)) {
    stop(
      "n_mutations must be integer or NA"
    )
  }

  # Only need to set the seed once
  set.seed(alignment_params$rng_seed)

  n_tries <- 1

  while (1) {
    alignment_dnabin <- create_alignment(
      phylogeny = phylogeny,
      alignment_params = alignment_params,
      n_mutations = NA,
      verbose = verbose
    )
    # if (beautier::is_site_model(alignment_params$site_model)) {
    #   # Standard site models
    #   alignment_dnabin <-  create_alignment_with_standard_site_model_raw(
    #     phylogeny = phylogeny,
    #     root_sequence = alignment_params$root_sequence,
    #     mutation_rate = alignment_params$mutation_rate,
    #     site_model = alignment_params$site_model
    #   )
    #   # alignment_dnabin <- create_alignment_with_standard_site_model(
    #   #   phylogeny = phylogeny,
    #   #   alignment_params = alignment_params
    #   # )
    # } else if (alignment_params$site_model == "linked_node_sub") {
    #   alignment_dnabin <- create_alignment_with_linked_node_sub_site_model(
    #     phylogeny = phylogeny,
    #     alignment_params = alignment_params
    #   )
    # } else {
    #   testit::assert(alignment_params$site_model == "unlinked_node_sub")
    #   alignment_dnabin <- create_alignment_with_unlinked_node_sub_site_model(
    #     phylogeny = phylogeny,
    #     alignment_params = alignment_params
    #   )
    # }

    testit::assert(class(alignment_dnabin) == "DNAbin")

    sim_mutations <- count_n_mutations(
      alignment = alignment_dnabin,
      root_sequence = alignment_params$root_sequence
    )

    testit::assert(
      get_alignment_sequence_length(alignment_dnabin) ==
      nchar(alignment_params$root_sequence)
    )

    if (verbose == TRUE) {
      print(
        paste0(
          "Mutations needed: ", n_mutations,
          ", got: ", sim_mutations,
          ", number of tries: ", n_tries
        )
      )
    }

    if (sim_mutations == n_mutations) break

    n_tries <- n_tries + 1
  }

  testit::assert(nrow(alignment_dnabin) == length(phylogeny$tip.label))
  testit::assert(
    ncol(alignment_dnabin) == nchar(alignment_params$root_sequence)
  )

  alignment_dnabin
}
