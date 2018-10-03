context("sim_alignment")

test_that("sim_alignment: basic", {
  n_taxa <- 3
  phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  testit::assert(length(phylogeny$tip.label) == n_taxa)
  sequence_length <- 10

  alignment <- sim_alignment(
    phylogeny = phylogeny,
    sequence_length = NULL,
    root_sequence = create_mono_nuc_dna(length = sequence_length),
    mutation_rate = 1
  )
  expect_true(class(alignment) == "DNAbin")
  expect_true(nrow(alignment) == n_taxa)
  expect_true(ncol(alignment) == sequence_length)
})

test_that("sim_alignment: abuse", {

  phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")

  expect_error(
    sim_alignment(
      phylogeny = "not a phylogeny",
      sequence_length = 2,
      mutation_rate = 1
    ),
    "'phylogeny' must be a phylogeny" #nolint
  )

  expect_error(
    sim_alignment(
      phylogeny = phylogeny,
      sequence_length = 10,
      root_sequence = "acgt",
      mutation_rate = 1
    ),
    "length of 'root_sequence' must equals 'sequence_length'"
  )

  expect_error(
    sim_alignment(
      phylogeny = phylogeny,
      sequence_length = 4,
      root_sequence = "XXXX",
      mutation_rate = 1
    ),
    "'root_sequence' must be a lowercase DNA sequence"
  )

  expect_error(
    sim_alignment(
      phylogeny = phylogeny,
      sequence_length = 2,
      root_sequence = "aa",
      mutation_rate = -1 # Must be positive
    ),
    "'mutation_rate' must be a non-zero and positive value" # nolint
  )

  set.seed(42)
  p_with_extant <- ape::rtree(5)
  testit::assert(geiger::is.extinct(p_with_extant))

  testthat::expect_error(
    sim_alignment(
      phylogeny = p_with_extant,
      sequence_length = 2,
      mutation_rate = 1
    ),
    "phylogeny must not contain extant species"
  )

})

test_that("new interface", {

  phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")

  expect_warning(
    sim_alignment(
      phylogeny = phylogeny,
      sequence_length = 4,
      root_sequence = "aaaa",
      mutation_rate = 0.1
    ),
    paste0(
      "'sequence_length' will be removed from the interface ",
      "in a future version. The number of characters in 'root_sequence' ",
      "will be used instead"
    )
  )

})
