#' Create an alignment with the \code{linked_node_sub} site model
#' @return an alignment of type \code{DNAbin}
#' @noRd
sim_twin_alignment_with_linked_node_sub_site_model <- function(
  phylogeny,
  alignment_params
) {
  beautier::check_phylogeny(phylogeny)
  pirouette::check_alignment_params(alignment_params)
  pirouette::check_reconstructed_phylogeny(phylogeny)
  testit::assert(alignment_params$site_model == "linked_node_sub")
  alignment_phydat <- nodeSub::sim_dual_linked(
    phylogeny,
    rootseq = strsplit(alignment_params$root_sequence, split = "")[[1]],
    l = nchar(alignment_params$root_sequence)
  )$alignment

  testthat::expect_equal(class(alignment_phydat), "phyDat")
  testit::assert(class(alignment_phydat) == "phyDat")

  alignment <- ape::as.DNAbin(alignment_phydat)
  alignment

}
