context("sim_alignment")

test_that("sim_alignment: basic", {
  n_taxa <- 5
  sequence_length <- 10
  phylogeny <- ape::rcoal(n_taxa)
  testit::assert(length(phylogeny$tip.label) == n_taxa)

  alignment <- sim_alignment(
    phylogeny = phylogeny,
    sequence_length = sequence_length,
    root_sequence = "aaaaaaaaaa",
    mutation_rate = 1
  )
  testthat::expect_true(class(alignment) == "DNAbin")
  testthat::expect_true(nrow(alignment) == n_taxa)
  testthat::expect_true(ncol(alignment) == sequence_length)
})

test_that("sim_alignment: abuse", {
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
      phylogeny = ape::rcoal(5),
      sequence_length = -1, # Must be positive
      mutation_rate = 1
    ),
    "'sequence_length' must be a non-zero and positive integer value" # nolint
  )

  expect_error(
    sim_alignment(
      phylogeny = ape::rcoal(5),
      sequence_length = 10,
      root_sequence = "acgt",
      mutation_rate = 1
    ),
    "length of 'root_sequence' must equals 'sequence_length'"
  )

  expect_error(
    sim_alignment(
      phylogeny = ape::rcoal(5),
      sequence_length = 4,
      root_sequence = "XXXX",
      mutation_rate = 1
    ),
    "'root_sequence' must be a lowercase DNA sequence"
  )

  expect_error(
    sim_alignment(
      phylogeny = ape::rcoal(5),
      sequence_length = 2,
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
