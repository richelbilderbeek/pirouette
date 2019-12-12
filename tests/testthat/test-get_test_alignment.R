test_that("returns an alignment", {
  expect_silent(check_alignment(get_test_alignment()))
})

test_that("use", {
  n_taxa <- 7
  sequence_length <- 11
  alignment <- get_test_alignment(
    n_taxa = n_taxa,
    sequence_length = sequence_length
  )
  expect_equal(n_taxa, get_alignment_n_taxa(alignment))
  expect_equal(
    sequence_length,
    get_alignment_sequence_length(alignment)
  )
  ape::image.DNAbin(alignment)
})
