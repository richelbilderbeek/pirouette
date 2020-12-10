# NO DOC YET, as NodeSub is not on CRAN yet
# Create an alignment with the \code{lns} site model
# @return an alignment of type \code{DNAbin}
# @inheritParams default_params_doc
# @seealso
# Use \link{sim_tral_with_uns_nsm}
# to simulate the true alignment with an unlinked node substitution model.
# Use \link{sim_twal_with_lns_nsm}
# to simulate the twin alignment with an linked node substitution model.
# Use \link{get_sim_tral_with_lns_nsm_fun}
# to get a partially evaluated unary function.
# @author Richèl J.C. Bilderbeek
# @export
# sim_tral_with_lns_nsm <- function(
#   true_phylogeny,
#   root_sequence,
#   subst_matrix = rep(1, 6),
#   branch_mutation_rate = 1.0,
#   node_mutation_rate = 1.0,
#   base_frequencies = rep(0.25, 4),
#   node_time = 0.001 # nolint allow commented code until nodeSub is on CRAN
# ) {
  # beautier::check_phylogeny(true_phylogeny) # nolint allow commented code until nodeSub is on CRAN
  # pirouette::check_root_sequence(root_sequence) # nolint allow commented code until nodeSub is on CRAN
  # pirouette::check_reconstructed_phylogeny(true_phylogeny) # nolint allow commented code until nodeSub is on CRAN
  # sim_result <- nodeSub::sim_linked(
  #   phy = true_phylogeny,
  #   Q = subst_matrix,
  #   rate = branch_mutation_rate,
  #   node_mut_rate_double = node_mutation_rate,
  #   bf = base_frequencies,
  #   rootseq = strsplit(root_sequence, split = "")[[1]],
  #   l = nchar(root_sequence),
  #   node_time = node_time # nolint allow commented code until nodeSub is on CRAN
  # )
  # testit::assert("alignment" %in% names(sim_result)) # nolint allow commented code until nodeSub is on CRAN
  # alignment_phydat <- sim_result$alignment # nolint allow commented code until nodeSub is on CRAN
  # testthat::expect_equal(class(alignment_phydat), "phyDat") # nolint allow commented code until nodeSub is on CRAN
  # alignment <- ape::as.DNAbin(alignment_phydat) # nolint allow commented code until nodeSub is on CRAN
  # pirouette::check_alignment(alignment) # nolint allow commented code until nodeSub is on CRAN
  # alignment
# }
