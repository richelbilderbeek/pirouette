test_that("basic", {
  n_taxa <- 3
  phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  testit::assert(length(phylogeny$tip.label) == n_taxa)
  sequence_length <- 10
  alignment_params <- create_alignment_params(
    root_sequence = create_mono_nuc_dna(length = sequence_length),
    mutation_rate = 1
  )

  alignment <- create_true_alignment(
    true_phylogeny = phylogeny,
    alignment_params = alignment_params
  )
  expect_equal(class(alignment), "DNAbin")
  expect_equal(nrow(alignment), n_taxa)
  expect_equal(ncol(alignment), sequence_length)
})

test_that("create_true_alignment: HKY", {
  n_taxa <- 3
  phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  testit::assert(length(phylogeny$tip.label) == n_taxa)
  sequence_length <- 10
  alignment_params <- create_alignment_params(
    root_sequence = create_mono_nuc_dna(length = sequence_length),
    mutation_rate = 1,
    site_model = beautier::create_hky_site_model()
  )

  alignment <- create_true_alignment(
    true_phylogeny = phylogeny,
    alignment_params = alignment_params
  )
  expect_true(class(alignment) == "DNAbin")
  expect_true(nrow(alignment) == n_taxa)
  expect_true(ncol(alignment) == sequence_length)
})

test_that("create_true_alignment: TN93", {
  n_taxa <- 3
  phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  testit::assert(length(phylogeny$tip.label) == n_taxa)
  sequence_length <- 10
  alignment_params <- create_alignment_params(
    root_sequence = create_mono_nuc_dna(length = sequence_length),
    mutation_rate = 1,
    site_model = beautier::create_tn93_site_model()
  )

  alignment <- create_true_alignment(
    true_phylogeny = phylogeny,
    alignment_params = alignment_params
  )
  expect_true(class(alignment) == "DNAbin")
  expect_true(nrow(alignment) == n_taxa)
  expect_true(ncol(alignment) == sequence_length)
})

test_that("create_true_alignment: GTR", {
  n_taxa <- 3
  phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  testit::assert(length(phylogeny$tip.label) == n_taxa)
  sequence_length <- 10
  alignment_params <- create_alignment_params(
    root_sequence = create_mono_nuc_dna(length = sequence_length),
    mutation_rate = 1,
    site_model = beautier::create_gtr_site_model()
  )

  alignment <- create_true_alignment(
    true_phylogeny = phylogeny,
    alignment_params = alignment_params
  )
  expect_true(class(alignment) == "DNAbin")
  expect_true(nrow(alignment) == n_taxa)
  expect_true(ncol(alignment) == sequence_length)
})

test_that("create_true_alignment: abuse", {

  phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  alignment_params <- create_alignment_params(
    root_sequence = create_mono_nuc_dna(length = 4),
    mutation_rate = 1
  )

  expect_error(
    create_true_alignment(
      true_phylogeny = "not a phylogeny",
      alignment_params = alignment_params
    ),
    "'true_phylogeny' must be a valid phylogeny" #nolint
  )

  expect_error(
    create_true_alignment(
      true_phylogeny = phylogeny,
      alignment_params = "nonsense"
    ),
    "'alignment_params' must be a set of alignment parameters"
  )

  set.seed(42)
  p_with_extant <- ape::rtree(5)
  testit::assert(geiger::is.extinct(p_with_extant))

  expect_error(
    create_true_alignment(
      true_phylogeny = p_with_extant,
      alignment_params = alignment_params
    ),
    "A reconstructed phylogeny must not contain extinct species"
  )

})

test_that("low mutation rate must have less mutations", {
  phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  sequence_length <- 1000
  root_sequence <- create_mono_nuc_dna(length = sequence_length)
  alignment_params_low <- create_alignment_params(
    root_sequence = root_sequence,
    mutation_rate = 0.01,
    rng_seed = 314
  )
  alignment_params_high <- create_alignment_params(
    root_sequence = root_sequence,
    mutation_rate = 0.1,
    rng_seed = 314
  )
  alignment_low <- create_true_alignment(
    true_phylogeny = phylogeny,
    alignment_params = alignment_params_low
  )
  alignment_high <- create_true_alignment(
    true_phylogeny = phylogeny,
    alignment_params = alignment_params_high
  )
  n_mutations_low <- count_n_mutations(
    alignment = alignment_low,
    root_sequence = root_sequence
  )
  n_mutations_high <- count_n_mutations(
    alignment = alignment_high,
    root_sequence = root_sequence
  )
  expect_true(n_mutations_low * 5 < n_mutations_high)
})
