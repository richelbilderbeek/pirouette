context("test-sim_alignment_file")

test_that("must create file", {
  alignment_params <- create_test_alignment_params()
  sim_alignment_file(
    phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
    alignment_params = alignment_params
  )
  expect_true(file.exists(alignment_params$fasta_filename))
})
