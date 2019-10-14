test_that("number of nucleotides must match", {
  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- create_alignment_params(
    site_model = "linked_node_sub",
    root_sequence = "acgt"
  )
  alignment <- create_alignment_with_linked_node_sub_site_model(
    phylogeny = phylogeny,
    alignment_params = alignment_params
  )
  expect_equal(nrow(alignment), ape::Ntip(phylogeny))

  skip("Issue #338")
  expect_equal(ncol(alignment), nchar(alignment_params$root_sequence))
})
