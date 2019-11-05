#' Create an alignment with the \code{linked_node_sub} site model
#' @inheritParams default_params_doc
#' @return an alignment of type \code{DNAbin}
#' @seealso
#' Use \link{sim_true_alignment_with_linked_node_sub_site_model}
#' to simulate the true alignment with a linked node substitution model.
#' Use \link{sim_twin_alignment_with_unlinked_node_sub_site_model}
#' to simulate the twin alignment with an unlinked node substitution model.
#' Use \link{get_sim_true_alignment_with_unlinked_node_sub_site_model_function}
#' to get a partially evaluated unary function.
#' @export
sim_true_alignment_with_unlinked_node_sub_site_model <- function(
  true_phylogeny,
  branch_subst_matrix = NULL,
  node_subst_matrix = 1.0,
  branch_mutation_rate = 1.0,
  node_mutation_rate = 1.0,
  base_frequencies = NULL,
  root_sequence,
  node_time = 0.001
) {
  beautier::check_phylogeny(true_phylogeny)
  pirouette::check_root_sequence(root_sequence)
  pirouette::check_reconstructed_phylogeny(true_phylogeny)
  alignment_phydat <- nodeSub::sim_dual_independent(
    phy = true_phylogeny,
    Q1 = branch_subst_matrix,
    Q2 = node_subst_matrix,
    rate1 = branch_mutation_rate,
    rate2 = node_mutation_rate,
    bf = base_frequencies,
    rootseq = strsplit(root_sequence, split = "")[[1]],
    l = nchar(root_sequence),
    node_time = node_time
  )$alignment

  testthat::expect_equal(class(alignment_phydat), "phyDat")
  testit::assert(class(alignment_phydat) == "phyDat")

  alignment <- ape::as.DNAbin(alignment_phydat)
  alignment

}
