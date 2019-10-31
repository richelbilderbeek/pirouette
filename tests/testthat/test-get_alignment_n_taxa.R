test_that("1 taxon, 2 nucleotides", {

  fasta_filename <- tempfile()
  writeLines(text = c(">A", "cc"), con = fasta_filename)
  alignment <- ape::read.FASTA(fasta_filename)
  expect_equal(1, get_alignment_n_taxa(alignment))
})

test_that("2 taxa, 3 nucleotides", {

  fasta_filename <- tempfile()
  writeLines(
    text = c(">A", "acg", ">B", "acg"),
    con = fasta_filename
  )
  alignment <- ape::read.FASTA(fasta_filename)
  expect_equal(2, get_alignment_n_taxa(alignment))
})


test_that("use, single taxon", {
  alignment <- ape::as.DNAbin(
    x = list(species_1 = strsplit("aagg", split = "")[[1]])
  )
  ape::image.DNAbin(alignment)
  expect_equal(1, get_alignment_n_taxa(alignment))
})

test_that("use, two taxa", {
  alignment <- ape::as.DNAbin(x = list(
      species_1 = strsplit("ac", split = "")[[1]],
      species_2 = strsplit("gt", split = "")[[1]]
    )
  )
  ape::image.DNAbin(alignment, show.bases = TRUE)
  expect_equal(2, get_alignment_n_taxa(alignment))
})

test_that("use, two taxa with 9 nucleotides", {
  alignment <- ape::as.DNAbin(x = list(
      species_1 = strsplit("aaaaccccg", split = "")[[1]],
      species_2 = strsplit("aaaattttt", split = "")[[1]]
    )
  )
  ape::image.DNAbin(alignment)
  expect_equal(2, get_alignment_n_taxa(alignment))
})

test_that("use, two taxa with 9 nucleotides, no mutation", {

  alignment <- ape::as.DNAbin(x = list(
      species_1 = strsplit("aaaaaaaaa", split = "")[[1]],
      species_2 = strsplit("aaaaaaaaa", split = "")[[1]]
    )
  )
  ape::image.DNAbin(alignment)
  expect_equal(2, get_alignment_n_taxa(alignment))
})

test_that("use, two taxa with 9 nucleotides, from FASTA", {
  fasta_filename <- tempfile()
  writeLines(
    text = c(">X", "AAAACCCCG", ">Y", "AAAATTTTT"),
    con = fasta_filename
  )
  alignment <- ape::read.FASTA(fasta_filename)
  ape::image.DNAbin(alignment)
  expect_equal(2, get_alignment_n_taxa(alignment))
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
  expect_equal(3, get_alignment_n_taxa(alignment))
})

test_that("use, three taxa, bug", {

  alignment <- ape::as.DNAbin(x = list(
      species_1 = strsplit("aggca", split = "")[[1]],
      species_2 = strsplit("agcca", split = "")[[1]],
      species_3 = strsplit("aacta", split = "")[[1]]
    )
  )
  expect_equal(3, get_alignment_n_taxa(alignment))
})

test_that("abuse", {

  expect_error(
    get_alignment_n_taxa(
      alignment = "nonsense"
    ),
    "'alignment' must be of class 'ape::DNAbin'"
  )
})



test_that("use three unlabelled taxa", {

  x <- list()
  x[[1]] <- rep("c", 4)
  x[[2]] <- rep("g", 4)
  x[[3]] <- rep("t", 4)
  alignment <- ape::as.DNAbin(x)
  expect_equal(3, get_alignment_n_taxa(alignment))
})


