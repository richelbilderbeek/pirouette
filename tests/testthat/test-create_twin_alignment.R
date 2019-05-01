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
  phylogeny_2 <- ape::read.tree(text = "(A:8, B:8);")
  ape::plot.phylo(phylogeny_1); ape::add.scale.bar(x = 0.0, y = 0.99, length = 2) # nolint too long, but used only temporarily
  ape::plot.phylo(phylogeny_2); ape::add.scale.bar(x = 0.0, y = 0.99, length = 2) # nolint too long, but used only temporarily



})
