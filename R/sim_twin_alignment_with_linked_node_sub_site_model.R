#' Adapter function to simulte the twin alignment
#' using the \code{linked_node_sub} site model
#' @inheritParams default_params_doc
#' @return an alignment of type \code{DNAbin}
#' @seealso
#' Use \link{sim_twin_alignment_with_unlinked_node_sub_site_model}
#' to simulate using an unlinked node substitution model.
#' Use \link{sim_true_alignment_with_linked_node_sub_site_model}
#' to simulate a true alignment.
#' @export
sim_twin_alignment_with_linked_node_sub_site_model <- function(
  twin_phylogeny,
  true_alignment = "irrelevant",
  root_sequence,
  subst_matrix = NULL,
  branch_mutation_rate = 1.0,
  node_mutation_rate = 1.0,
  base_frequencies = NULL,
  node_time = 0.001
) {
  beautier::check_phylogeny(twin_phylogeny)
  pirouette::check_reconstructed_phylogeny(twin_phylogeny)
  pirouette::check_root_sequence(root_sequence)
  nodesub_result <- nodeSub::sim_dual_linked(
    phy = twin_phylogeny,
    Q = subst_matrix,
    rate = branch_mutation_rate,
    node_mut_rate_double = node_mutation_rate,
    bf = base_frequencies,
    rootseq = strsplit(root_sequence, split = "")[[1]],
    l = nchar(root_sequence),
    node_time = node_time
  )
  testit::assert("alignment" %in% names(nodesub_result))
  alignment_phydat <- nodesub_result$alignment
  testthat::expect_equal(class(alignment_phydat), "phyDat")
  testit::assert(class(alignment_phydat) == "phyDat")
  alignment <- ape::as.DNAbin(alignment_phydat)
  alignment
}
