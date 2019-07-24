test_that("1 taxon, 2 nucleotides", {

  fasta_filename <- tempfile()
  writeLines(text = c(">A", "cc"), con = fasta_filename)
  alignment <- ape::read.FASTA(fasta_filename)
  expect_equal(2, get_alignment_sequence_length(alignment))
})

test_that("2 taxa, 3 nucleotides", {

  fasta_filename <- tempfile()
  writeLines(
    text = c(">A", "acg", ">B", "acg"),
    con = fasta_filename
  )
  alignment <- ape::read.FASTA(fasta_filename)
  expect_equal(3, get_alignment_sequence_length(alignment))
})
