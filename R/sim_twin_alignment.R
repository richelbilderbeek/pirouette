#' Creates a twin alignment.
#'
#' A twin alignment is an alignment that has as much acquired
#' the same number of mutations (compared to the root sequence),
#' as the true alingment has (compared to the root sequence).
#'
#' When an alignment gets very big, say one million base pairs,
#' it will take long to get a twin alignment with exactly the same
#' number of mutations.
#' @inheritParams default_params_doc
#' @return an alignment of class DNAbin that has as much
#'   mutations accumulated from crown to the tips as the
#'   original, 'true' alignment
#' @examples
#' true_phylogeny <- ape::read.tree(text = "((A:1, B:1):2, C:3);")
#' twin_phylogeny <- ape::read.tree(text = "((A:2, B:2):1, C:3);")
#' root_sequence <- create_blocked_dna(1000)
#' alignment_params <- create_test_alignment_params(
#'   root_sequence = root_sequence,
#'   rng_seed = 314,
#'   mutation_rate = 0.001
#' )
#' true_alignment <- create_true_alignment(
#'   true_phylogeny = true_phylogeny,
#'   alignment_params = alignment_params
#' )
#' n_mutations_true <- count_n_mutations(
#'   alignment = true_alignment, root_sequence = root_sequence
#' )
#'
#' twin_alignment <- sim_twin_alignment(
#'   twin_phylogeny = twin_phylogeny,
#'   true_alignment = true_alignment,
#'   alignment_params = alignment_params,
#'   twinning_params = create_twinning_params()
#' )
#' n_mutations_twin <- count_n_mutations(
#'   alignment = twin_alignment, root_sequence = root_sequence
#' )
#'
#' library(testthat)
#' expect_equal(n_mutations_true, n_mutations_twin)
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @export
sim_twin_alignment <- function(
  twin_phylogeny,
  true_alignment,
  alignment_params, # To be obsoleted
  twinning_params
) {
  beautier::check_phylogeny(twin_phylogeny)
  pirouette::check_alignment(true_alignment)
  pirouette::check_alignment_params(alignment_params)
  pirouette::check_twinning_params(twinning_params)

  set.seed(twinning_params$rng_seed_twin_alignment)
  return(
    twinning_params$sim_twin_alignment_function(
      twin_phylogeny = twin_phylogeny,
      true_alignment = true_alignment,
      root_sequence = alignment_params$root_sequence
    )
  )
}
