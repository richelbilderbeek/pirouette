context("create_mono_nuc_dna")

test_that("use", {
  expect_equal(create_mono_nuc_dna(nucleotide = "a", length = 1), "a")
  expect_equal(create_mono_nuc_dna(nucleotide = "c", length = 2), "cc")
  expect_equal(create_mono_nuc_dna(nucleotide = "g", length = 3), "ggg")
  expect_equal(create_mono_nuc_dna(nucleotide = "t", length = 4), "tttt")
})

test_that("abuse", {
  expect_error(
    create_mono_nuc_dna(nucleotide = "x", length = 4),
    "'nucleotide' must be a lowercase nucleotide character"
  )
  expect_error(
    create_mono_nuc_dna(length = "nonsense"),
    "'length' must be numerical"
  )
  expect_error(
    create_mono_nuc_dna(length = -1234),
    "'length' must be a positive non-zero number"
  )
})

