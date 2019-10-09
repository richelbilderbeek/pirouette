#' Converts a phylogeny to a random DNA alignment
#' @inheritParams default_params_doc
#' @param n_mutations the number of different base pairs between
#' root sequence and the resulting alignment
#' @return an alignment of type \code{DNAbin}
#' @seealso Use \link{create_alignment_file} to save the simulated alignment
#'   directly to a file
#' @examples
#' library(testthat)
#'
#' # Create the ancestor's DNA sequence
#' n_base_pairs <- 4
#' root_sequence <- create_blocked_dna(length = n_base_pairs)
#' mutation_rate <- 0.1
#' n_taxa <- 5
#' rng_seed <- 314
#' phylogeny <- ape::rcoal(n_taxa)
#' site_model <- create_jc69_site_model()
#'
#' # Simulate the alignment
#' alignment <- create_alignment_impl(
#'    phylogeny = phylogeny,
#'    root_sequence = root_sequence,
#'    rng_seed = rng_seed,
#'    mutation_rate = mutation_rate,
#'    site_model = site_model
#'  )
#'
#' expect_equal(class(alignment), "DNAbin")
#' expect_equal(nrow(alignment), n_taxa)
#' expect_equal(ncol(alignment), n_base_pairs)
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_alignment_impl <- function(
  phylogeny,
  root_sequence,
  rng_seed,
  mutation_rate,
  site_model,
  n_mutations = NA,
  verbose = FALSE
) {
  beautier::check_phylogeny(phylogeny)
  pirouette::check_reconstructed_phylogeny(phylogeny)
  testit::assert(beautier::is_one_int(rng_seed))
  testit::assert(
    is.function(mutation_rate) ||
    beautier::is_one_double(mutation_rate)
  )
  beautier::check_site_model(site_model)
  testit::assert(
    beautier::is_one_int(n_mutations) ||
    beautier::is_one_na(n_mutations)
  )
  testit::assert(
    beautier::is_one_na(n_mutations) ||
    n_mutations >= 0
  )
  testit::assert(beautier::is_one_bool(verbose))

  # If mutation_rate is function, apply it to the phylogeny
  if (is.function(mutation_rate)) {
    mutation_function <- mutation_rate
    mutation_rate <- mutation_function(phylogeny)
  }
  testit::assert(mutation_rate >= 0.0)
  if (!beautier::is_one_na(n_mutations) && n_mutations > 0) {
    testit::assert(mutation_rate > 0.0)
  }

  if (!beautier::is_one_int(n_mutations) && !beautier::is_one_na(n_mutations)) {
    stop(
      "n_mutations must be integer or NA"
    )
  }

  # Only need to set the seed once
  set.seed(rng_seed)

  n_tries <- 1

  while (1) {
    alignment_phydat <- phangorn::simSeq(
      phylogeny,
      l = nchar(root_sequence),
      rate = mutation_rate,
      rootseq = strsplit(root_sequence, split = "")[[1]],
      Q = create_rate_matrix(
        site_model = site_model,
        base_frequencies = calc_base_freq(root_sequence)
      )
    )

    testit::assert(class(alignment_phydat) == "phyDat")

    alignment_dnabin <- ape::as.DNAbin(alignment_phydat)

    testit::assert(class(alignment_dnabin) == "DNAbin")

    sim_mutations <- count_n_mutations(
      alignment = alignment_dnabin,
      root_sequence = root_sequence
    )

    if (beautier::is_one_na(n_mutations)) break

    testit::assert(
      get_alignment_sequence_length(alignment_dnabin) ==
      nchar(root_sequence)
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
  testit::assert(ncol(alignment_dnabin) == nchar(root_sequence))

  alignment_dnabin
}
