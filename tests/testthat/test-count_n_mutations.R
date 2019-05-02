context("test-count_n_mutations")

test_that("use, single taxon", {
  skip("count_n_mutations")
  #
  # Root sequence is known:AAAA
  #
  # AAAA -> AAGG                                                                # nolint this is no commented code
  #
  # Those are two mutations
  #
  # Don't forget: ape assumes lowercase
  root_sequence <- "aaaa"
  alignment <- ape::as.DNAbin(
    x = list(species_1 = strsplit("aagg", split = "")[[1]])
  )
  ape::image.DNAbin(alignment)
  expect_equal(
    count_n_mutation(alignment = alignment, root_sequence = root_sequence),
    2
  )
})

test_that("use, two taxa", {
  skip("count_n_mutations")
  #
  # Root sequence is known
  #
  #           +---- AAAACCCC 4 mutations
  # AAAAAAAA -+
  #           +---- AAAATTTT 4 mutations
  #                          ----------- +
  #                          8 mutations
  #
  # Those are eight mutations in total
  #
  # Don't forget: ape assumes lowercase
  root_sequence <- "aaaaaaaa"
  alignment <- ape::as.DNAbin(x = list(
      species_1 = strsplit("aaaacccc", split = "")[[1]],
      species_2 = strsplit("aaaatttt", split = "")[[1]]
    )
  )
  ape::image.DNAbin(alignment)
  expect_equal(
    count_n_mutation(alignment = alignment, root_sequence = root_sequence),
    8
  )
})

test_that("use, three taxa", {
  skip("count_n_mutations")
  #
  # Root sequence is known
  #
  #           +---- AAAACCCC 4 mutations
  # AAAAAAAA -+
  #           | +-- AAAATTTT 4 mutations
  #           +-+
  #             +-- AAAATTGG 4 mutations
  #                          ----------- +
  #                         12 mutations
  #
  # Those are twelve mutations in total
  #
  # Don't forget: ape assumes lowercase
  root_sequence <- "aaaaaaaa"
  alignment <- ape::as.DNAbin(x = list(
      species_1 = strsplit("aaaacccc", split = "")[[1]],
      species_2 = strsplit("aaaatttt", split = "")[[1]],
      species_3 = strsplit("aaaattgg", split = "")[[1]]
    )
  )
  ape::image.DNAbin(alignment)
  expect_equal(
    count_n_mutation(alignment = alignment, root_sequence = root_sequence),
    12
  )
})


test_that("abuse", {
  skip("count_n_mutations")

  expect_error(
    count_n_mutation(
      alignment = "nonsense",
      root_sequence = "aaaa"
    ),
    "'alignment' must be of class 'ape::DNAbin'"
  )

  expect_error(
    count_n_mutation(
      alignment = ape::as.DNAbin(
        x = list(species_1 = strsplit("aaaa", split = "")[[1]])
      ),
      root_sequence = "nonsense"
    ),
    "'root_sequence' must be one character vector of lowercase nucleotides"
  )

  root_sequence <- "aaaa"
  alignment <- ape::as.DNAbin(
    x = list(species_1 = strsplit("aaaaaaaaaaaaaaaaaaaaaaaaa", split = "")[[1]])
  )
  expect_error(
    count_n_mutation(
      alignment = alignment,
      root_sequence = root_sequence
    ),
    "'root_sequence' must have the same length as the alignments' taxa"
  )
})
