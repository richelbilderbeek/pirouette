# nodeSub is not on CRAN yet

# test_that("number of nucleotides must match", {
#
#   phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
#   root_sequence <- "acgt"
#   alignment <- pirouette::sim_tral_with_lns_nsm(
#     true_phylogeny = phylogeny,
#     root_sequence = root_sequence
#   )
#   expect_equal(nrow(alignment), ape::Ntip(phylogeny))
#   expect_equal(ncol(alignment), nchar(root_sequence))
# })
