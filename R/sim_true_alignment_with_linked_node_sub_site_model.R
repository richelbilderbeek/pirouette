#' Create an alignment with the \code{linked_node_sub} site model
#' @return an alignment of type \code{DNAbin}
#' @seealso
#' Use \link{sim_true_alignment_with_unlinked_node_sub_site_model}
#' to simulate the true alignment with an unlinked node substitution model.
#' Use \link{sim_twin_alignment_with_linked_node_sub_site_model}
#' to simulate the twin alignment with an linked node substitution model.
#' @export
sim_true_alignment_with_linked_node_sub_site_model <- function(
  true_phylogeny,
  root_sequence,
  site_model = "linked_node_sub"
) {
  beautier::check_phylogeny(true_phylogeny)
  pirouette::check_root_sequence(root_sequence)
  pirouette::check_reconstructed_phylogeny(true_phylogeny)
  testit::assert(site_model == "linked_node_sub")
  sim_result <- nodeSub::sim_dual_linked(
    true_phylogeny,
    rootseq = strsplit(root_sequence, split = "")[[1]],
    l = nchar(root_sequence)
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
  check_alignment(alignment)
  alignment

}
