test_that("number of nucleotides must match", {
  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  root_sequence <- "acgt"
  alignment <- sim_true_alignment_with_linked_node_sub_site_model(
    true_phylogeny = phylogeny,
    root_sequence = root_sequence
  )
  expect_equal(nrow(alignment), ape::Ntip(phylogeny))
  expect_equal(ncol(alignment), nchar(root_sequence))
})
