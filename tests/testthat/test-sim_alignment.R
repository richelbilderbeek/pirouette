context("test-sim_alignment")

test_that("sim_alignment: basic", {
  n_taxa <- 3
  phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  testit::assert(length(phylogeny$tip.label) == n_taxa)
  sequence_length <- 10
  alignment_params <- create_alignment_params(
    root_sequence = create_mono_nuc_dna(length = sequence_length),
    mutation_rate = 1
  )

  alignment <- sim_alignment(
    phylogeny = phylogeny,
    alignment_params = alignment_params
  )
  expect_true(class(alignment) == "DNAbin")
  expect_true(nrow(alignment) == n_taxa)
  expect_true(ncol(alignment) == sequence_length)
})

test_that("sim_alignment: abuse", {

  phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  alignment_params <- create_alignment_params(
    root_sequence = create_mono_nuc_dna(length = 4),
    mutation_rate = 1
  )

  expect_error(
    sim_alignment(
      phylogeny = "not a phylogeny",
      alignment_params = alignment_params
    ),
    "'phylogeny' must be a phylogeny" #nolint
  )

  expect_error(
    sim_alignment(
      phylogeny = phylogeny,
      alignment_params = "nonsense"
    ),
    "'alignment_params' must be a set of alignment parameters"
  )

  set.seed(42)
  p_with_extant <- ape::rtree(5)
  testit::assert(geiger::is.extinct(p_with_extant))

  expect_error(
    sim_alignment(
      phylogeny = p_with_extant,
      alignment_params = alignment_params
    ),
    "phylogeny must not contain extant species"
  )

})
