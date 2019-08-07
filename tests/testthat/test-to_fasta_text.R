test_that("use", {

  n_taxa <- 4

  alignment <- create_true_alignment(
     true_phylogeny = ape::rcoal(n_taxa),
     alignment_params = create_test_alignment_params()
   )

  expect_silent(to_fasta_text(alignment))
  expect_equal(n_taxa * 2, length(to_fasta_text(alignment)))
})
