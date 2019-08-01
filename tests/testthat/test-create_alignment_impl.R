test_that("use", {
  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  root_sequence <- "acgt"
  rng_seed <- 314
  mutation_rate <- 0.1
  site_model <- beautier::create_jc69_site_model()
  n_mutations <- NA

  expect_silent(
    create_alignment_impl(
      phylogeny = phylogeny,
      root_sequence = root_sequence,
      rng_seed = rng_seed,
      mutation_rate = mutation_rate,
      site_model = site_model,
      n_mutations = n_mutations
    )
  )
  # It's an impl, so error messages matter less
  expect_error(
    create_alignment_impl(
      phylogeny = "nonsense",
      root_sequence = root_sequence,
      rng_seed = rng_seed,
      mutation_rate = mutation_rate,
      site_model = site_model,
      n_mutations = n_mutations
    )
  )
  expect_error(
    create_alignment_impl(
      phylogeny = phylogeny,
      root_sequence = "nonsense",
      rng_seed = rng_seed,
      mutation_rate = mutation_rate,
      site_model = site_model,
      n_mutations = n_mutations
    )
  )
  expect_error(
    create_alignment_impl(
      phylogeny = phylogeny,
      root_sequence = root_sequence,
      rng_seed = "nonsense",
      mutation_rate = mutation_rate,
      site_model = site_model,
      n_mutations = n_mutations
    )
  )
  expect_error(
    create_alignment_impl(
      phylogeny = phylogeny,
      root_sequence = root_sequence,
      rng_seed = rng_seed,
      mutation_rate = "nonsense",
      site_model = site_model,
      n_mutations = n_mutations
    )
  )
  expect_error(
    create_alignment_impl(
      phylogeny = phylogeny,
      root_sequence = root_sequence,
      rng_seed = rng_seed,
      mutation_rate = mutation_rate,
      site_model = "nonsense",
      n_mutations = n_mutations
    )
  )
  expect_error(
    create_alignment_impl(
      phylogeny = phylogeny,
      root_sequence = root_sequence,
      rng_seed = rng_seed,
      mutation_rate = mutation_rate,
      site_model = site_model,
      n_mutations = "nonsense"
    )
  )
  expect_error(
    create_alignment_impl(
      phylogeny = phylogeny,
      root_sequence = root_sequence,
      rng_seed = rng_seed,
      mutation_rate = 0.0,
      site_model = site_model,
      n_mutations = 100
    )
  )
})
