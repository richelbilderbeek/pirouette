#' Create an alignment with the \code{lns} site model
#' @return an alignment of type \code{DNAbin}
#' @inheritParams default_params_doc
#' @seealso
#' Use \link{sim_true_alignment_with_uns_site_model}
#' to simulate the true alignment with an unlinked node substitution model.
#' Use \link{sim_twin_alignment_with_lns_site_model}
#' to simulate the twin alignment with an linked node substitution model.
#' Use \link{get_sim_true_alignment_with_lns_site_model_fun}
#' to get a partially evaluated unary function.
#' @export
sim_true_alignment_with_lns_site_model <- function(
  true_phylogeny,
  root_sequence,
  subst_matrix = NULL,
  branch_mutation_rate = 1.0,
  node_mutation_rate = 1.0,
  base_frequencies = NULL,
  node_time = 0.001
) {
  beautier::check_phylogeny(true_phylogeny)
  pirouette::check_root_sequence(root_sequence)
  pirouette::check_reconstructed_phylogeny(true_phylogeny)
  sim_result <- nodeSub::sim_dual_linked(
    phy = true_phylogeny,
    Q = subst_matrix,
    rate = branch_mutation_rate,
    node_mut_rate_double = node_mutation_rate,
    bf = base_frequencies,
    rootseq = strsplit(root_sequence, split = "")[[1]],
    l = nchar(root_sequence),
    node_time = node_time
  )
  testit::assert("alignment" %in% names(sim_result))
  alignment_phydat <- sim_result$alignment

  if (class(alignment_phydat) != "phyDat") {
    stop(
      "Result of nodeSub::sim_dual_linked(...)$alignment",
      " must be of class phyDat. ",
      "Actual class: ", class(alignment_phydat), " \n",
      "Actual value: ", alignment_phydat, " \n",
      "Complete result of 'nodeSub::sim_dual_linked': ", sim_result, " \n"
    )
  }
  testthat::expect_equal(class(alignment_phydat), "phyDat")
  testit::assert(class(alignment_phydat) == "phyDat")

  alignment <- ape::as.DNAbin(alignment_phydat)
  pirouette::check_alignment(alignment)
  alignment

}
