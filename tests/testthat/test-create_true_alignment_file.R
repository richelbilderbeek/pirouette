context("test-create_true_alignment_file")

test_that("must create file", {
  alignment_params <- create_test_alignment_params()
  create_true_alignment_file(
    phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
    alignment_params = alignment_params
  )
  expect_true(file.exists(alignment_params$fasta_filename))
  expect_s3_class(
    ape::read.FASTA(alignment_params$fasta_filename),
    "DNAbin"
  )
})

test_that("must create file in subsub folder", {
  alignment_params <- create_test_alignment_params()
  alignment_params$fasta_filename <- file.path(
    tempdir(), "a", "b", "c", "d", "e.fasta"
  )
  create_true_alignment_file(
    phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
    alignment_params = alignment_params
  )
  expect_true(file.exists(alignment_params$fasta_filename))
})
