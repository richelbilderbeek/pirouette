test_that("use", {
  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- create_alignment_params()
  n_mutations <- NA

  expect_silent(
    create_alignment(
      phylogeny = phylogeny,
      alignment_params = alignment_params,
      n_mutations = n_mutations
    )
  )
  # It's an impl, so error messages matter less
  expect_error(
    create_alignment(
      phylogeny = "nonsense",
      alignment_params = alignment_params,
      n_mutations = n_mutations
    )
  )
  expect_error(
    create_alignment(
      phylogeny = phylogeny,
      alignment_params = "nonsense",
      n_mutations = n_mutations
    )
  )
  expect_error(
    create_alignment(
      phylogeny = phylogeny,
      root_sequence = root_sequence,
      rng_seed = rng_seed,
      mutation_rate = mutation_rate,
      site_model = site_model,
      n_mutations = "nonsense"
    )
  )
  expect_error(
    create_alignment(
      phylogeny = phylogeny,
      alignment_params = alignment_params,
      n_mutations = 1e123,
      verbose = TRUE
    ),
    "Cannot have more mutations than the total number of nucleotides"
  )
})
