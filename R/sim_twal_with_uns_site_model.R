#' Adapter function to simulate an alignment
#' with the \code{linked_node_sub} (lns) site model.
#' @inheritParams default_params_doc
#' @return an alignment of type \code{DNAbin}
#' @seealso
#' Use \link{sim_twal_with_lns_nsm}
#' to simulate the true alignment with a linked node substitution model.
#' Use \link{sim_tral_with_uns_nsm}
#' to simulate the twin alignment with an unlinked node substitution model.
#' @export
sim_twal_with_uns_nsm <- function(
  twin_phylogeny,
  root_sequence,
  true_alignment = "irrelevant",
  branch_subst_matrix = rep(1, 6),
  node_subst_matrix = 1.0,
  branch_mutation_rate = 1.0,
  node_mutation_rate = 1.0,
  base_frequencies = rep(0.25, 4),
  node_time = 0.001
) {
  beautier::check_phylogeny(twin_phylogeny)
  pirouette::check_root_sequence(root_sequence)
  pirouette::check_reconstructed_phylogeny(twin_phylogeny)
  alignment_phydat <- nodeSub::sim_dual_independent(
    phy = twin_phylogeny,
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
