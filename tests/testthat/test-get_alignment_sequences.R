context("test-get_alignment_sequences")

test_that("use, single taxon", {
  # Don't forget: ape assumes lowercase

  alignment <- ape::as.DNAbin(
    x = list(species_1 = strsplit("aagg", split = "")[[1]])
  )
  ape::image.DNAbin(alignment)
  expect_equal(
    get_alignment_sequences(alignment),
    "aagg"
  )
})

test_that("use, two taxa", {
  alignment <- ape::as.DNAbin(x = list(
      species_1 = strsplit("ac", split = "")[[1]],
      species_2 = strsplit("gt", split = "")[[1]]
    )
  )
  ape::image.DNAbin(alignment, show.bases = TRUE)
  expect_equal(
    get_alignment_sequences(alignment, verbose = TRUE),
    c("ac", "gt")
  )
})

test_that("use, two taxa with 9 nucleotides", {
  alignment <- ape::as.DNAbin(x = list(
      species_1 = strsplit("aaaaccccg", split = "")[[1]],
      species_2 = strsplit("aaaattttt", split = "")[[1]]
    )
  )
  ape::image.DNAbin(alignment)
  expect_equal(
    get_alignment_sequences(alignment),
    c("aaaaccccg", "aaaattttt")
  )
})

test_that("use, two taxa with 9 nucleotides, no mutation", {

  alignment <- ape::as.DNAbin(x = list(
      species_1 = strsplit("aaaaaaaaa", split = "")[[1]],
      species_2 = strsplit("aaaaaaaaa", split = "")[[1]]
    )
  )
  ape::image.DNAbin(alignment)
  expect_equal(
    get_alignment_sequences(alignment),
    c("aaaaaaaaa", "aaaaaaaaa")
  )
})

test_that("use, two taxa with 9 nucleotides, from FASTA", {
  fasta_filename <- tempfile()
  writeLines(
    text = c(">X", "AAAACCCCG", ">Y", "AAAATTTTT"),
    con = fasta_filename
  )
  alignment <- ape::read.FASTA(fasta_filename)
  ape::image.DNAbin(alignment)
  expect_equal(
    get_alignment_sequences(alignment),
    c("aaaaccccg", "aaaattttt")
  )
})

test_that("use, three taxa", {
  root_sequence <- "aaaaaaaa"
  alignment <- ape::as.DNAbin(x = list(
      species_1 = strsplit("aaaacccc", split = "")[[1]],
      species_2 = strsplit("aaaatttt", split = "")[[1]],
      species_3 = strsplit("aaaattgg", split = "")[[1]]
    )
  )
  ape::image.DNAbin(alignment)
  expect_equal(
    get_alignment_sequences(alignment),
    c("aaaacccc", "aaaatttt", "aaaattgg")
  )
})

test_that("use, three taxa, bug", {

  skip("Issue 298, Issue #298")
  alignment <- ape::as.DNAbin(x = list(
      species_1 = strsplit("aggca", split = "")[[1]],
      species_2 = strsplit("agcca", split = "")[[1]],
      species_3 = strsplit("aacta", split = "")[[1]]
    )
  )
  ape::image.DNAbin(alignment, main = root_sequence, show.bases = TRUE)
  expect_equal(
    get_alignment_sequences(alignment),
    c("aggca", "agcca", "aacta")
  )
})

test_that("abuse", {

  expect_error(
    get_alignment_sequences(
      alignment = "nonsense"
    ),
    "'alignment' must be of class 'ape::DNAbin'"
  )
})
