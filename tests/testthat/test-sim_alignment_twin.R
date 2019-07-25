context("test-sim_alignment_twin")

test_that("use, two taxa", {

  # If we can create an alignment with any desired number of
  # mutations, we can create a twin alignment:
  # just create an alignmnet with the same number of mutations
  # as the true alignment

  n_mutations <- 8
  root_sequence <- "aaaaaaaa"

  alignment <- sim_alignment_twin(
    twin_phylogeny = ape::read.tree(text = "(A:4, B:4);"),
    root_sequence = root_sequence,
    rng_seed_twin_alignment = 314,
    mutation_rate = 0.125,
    site_model = create_jc69_site_model(),
    n_mutations = n_mutations
  )
  expect_equal(
    count_n_mutations(alignment, root_sequence = root_sequence),
    n_mutations
  )
})

test_that("use, three taxa", {

  n_mutations <- 12
  root_sequence <- "aaaaaaaa"

  alignment <- sim_alignment_twin(
    twin_phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
    root_sequence = root_sequence,
    rng_seed_twin_alignment = 314,
    mutation_rate = 0.25,
    site_model = create_jc69_site_model(),
    n_mutations = n_mutations
  )
  expect_equal(
    count_n_mutations(alignment, root_sequence = root_sequence),
    n_mutations
  )
})

test_that("use, starting from a true phylogeny, #294", {

  alignment_params <- create_test_alignment_params(
    root_sequence = create_blocked_dna(length = 1000),
    mutation_rate = 0.1
  )
  twinning_params <- create_twinning_params(
    rng_seed_twin_alignment = 314
  )
  true_phylogeny <- ape::read.tree(text = "((A:4, B:4):1, C:5);")
  twin_phylogeny <- ape::read.tree(text = "((A:1, B:1):4, C:5);")
  true_alignment <- sim_alignment(
    phylogeny = true_phylogeny,
    alignment_params = alignment_params,
    n_mutations = NA
  )
  n_mutations <- count_n_mutations(
    alignment = true_alignment,
    root_sequence = alignment_params$root_sequence
  )
  twin_alignment <- sim_alignment_twin(
    twin_phylogeny = twin_phylogeny,
    root_sequence = alignment_params$root_sequence,
    rng_seed_twin_alignment = twinning_params$rng_seed_twin_alignment,
    mutation_rate = alignment_params$mutation_rate,
    site_model = alignment_params$site_model,
    n_mutations = n_mutations,
    verbose = FALSE
  )
  n_mutations_twin <- count_n_mutations(
    alignment = twin_alignment,
    root_sequence = alignment_params$root_sequence
  )
  expect_equal(n_mutations, n_mutations_twin)
})
