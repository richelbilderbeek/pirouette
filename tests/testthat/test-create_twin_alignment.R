context("test-create_twin_alignment")

test_that("use", {

  skip("create_twin_alignment")
  # Two different phylogenies must have the same number of mutations
  #
  #  Phylogeny 1:
  #
  #  +---- 1                                                                    # nolint this is no commented code
  # -+
  #  +---- 2                                                                    # nolint this is no commented code
  #
  #  Phylogeny 2:
  #
  #  +-------- 1                                                                # nolint this is no commented code
  # -+
  #  +-------- 2                                                                # nolint this is no commented code
  #
  # Root sequence is known, e.g. AAAAAAAA
  #
  #  Phylogeny 1:
  #
  #          +---- AAAACCCC                                                     # nolint this is no commented code
  # AAAAAAAA-+
  #          +---- AAAAGGGG                                                     # nolint this is no commented code
  #
  #
  #  Phylogeny 2:
  #
  #          +-------- GGGGAAAA                                                 # nolint this is no commented code
  # AAAAAAAA-+
  #          +-------- TTTTAAAA                                                 # nolint this is no commented code
  #
  #

  phylogeny_1 <- ape::read.tree(text = "(A:4, B:4);")
  root_sequence <- "aaaaaaaa"
  alignment_1 <- ape::as.DNAbin(x = list(
      species_1 = strsplit("aaaacccc", split = "")[[1]],
      species_2 = strsplit("aaaatttt", split = "")[[1]]
    )
  )
  ape::image.DNAbin(alignment)
  expect_equal(
    count_n_mutation(alignment = alignment, root_sequence = root_sequence),
    8
  )

  phylogeny_2 <- ape::read.tree(text = "(A:8, B:8);")
  ape::plot.phylo(phylogeny_1); ape::add.scale.bar(x = 0.0, y = 0.99, length = 2) # nolint too long, but used only temporarily
  ape::plot.phylo(phylogeny_2); ape::add.scale.bar(x = 0.0, y = 0.99, length = 2) # nolint too long, but used only temporarily

  expect_equal(
    count_n_mutation(alignment = alignment_1, root_sequence = root_sequence),
    count_n_mutation(alignment = alignment_2, root_sequence = root_sequence)
  )

  create_twin_alignment(
    root_sequence = root_sequence,
    phylogeny,
    n_mutations

  )


})
