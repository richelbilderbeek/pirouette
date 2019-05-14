context("test-count_n_mutations")

test_that("use, single taxon", {

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
    count_n_mutations(alignment = alignment, root_sequence = root_sequence),
    2
  )
})

test_that("use, two taxa", {

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
    count_n_mutations(alignment = alignment, root_sequence = root_sequence),
    8
  )
})

test_that("use, three taxa", {

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
    count_n_mutations(alignment = alignment, root_sequence = root_sequence),
    12
  )
})


test_that("abuse", {

  expect_error(
    count_n_mutations(
      alignment = "nonsense",
      root_sequence = "aaaa"
    ),
    "'alignment' must be of class 'ape::DNAbin'"
  )

  expect_error(
    count_n_mutations(
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
    count_n_mutations(
      alignment = alignment,
      root_sequence = root_sequence
    ),
    "'root_sequence' must have the same length as each taxon's sequence length"
  )
})

test_that("Bug #257", {

  true_tree <- ape::read.tree(text = "((A:1, B:1):2, C:3);")
  root_sequence <- create_blocked_dna(1000)
  true_alignment <- sim_alignment(
    phylogeny = true_tree,
    alignment_params = create_test_alignment_params(
      root_sequence = root_sequence
    )
  )
  expect_silent(
    count_n_mutations(
      alignment = true_alignment, root_sequence = root_sequence
    )
  )
})
