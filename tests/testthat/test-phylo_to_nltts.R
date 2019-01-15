context("test-phylo_to_nltts")

test_that("use", {

  n_base_pairs <- 4
  seed <- 1
  phylogeny <- load_tree(model = "mbd", seed = seed)

  nltts <- phylo_to_errors(
    phylogeny = phylogeny,
    alignment_params = create_alignment_params(
      root_sequence = "acgt",
      mutation_rate = 0,
      site_model = beautier::create_jc69_site_model(),
      clock_model = beautier::create_strict_clock_model(),
      rng_seed = 0
    ),
    site_model = beautier::create_jc69_site_model(),
    clock_model = beautier::create_strict_clock_model(),
    tree_prior = beautier::create_tree_prior_bd(),
    inference_param = create_inference_param(
      mcmc = beautier::create_mcmc(chain_length = 2000)
    )
  )

  nltts <- phylo_to_errors(
    phylogeny = phylogeny,
    alignment_params = create_alignment_params(),
    site_model = beautier::create_jc69_site_model(),
    clock_model = beautier::create_strict_clock_model(),
    tree_prior = beautier::create_tree_prior_bd(),
    inference_param = create_inference_param(
      mcmc = beautier::create_mcmc(chain_length = 2000)
    )
  )

  expect_true(
    length(nltts) > 0
  )
  expect_true(
    all(nltts > 0) & all(nltts < 1)
  )
})

test_that("abuse", {

  mcmc <- create_mcmc(chain_length = 2000)
  n_base_pairs <- 4
  seed <- 1
  phylogeny <- load_tree(model = "mbd", seed = seed)

  expect_error(
    phylo_to_nltts(
      phylogeny = c(3, 2, 1),
      mcmc = mcmc,
      n_base_pairs = n_base_pairs,
      seed = seed
    ),
    "parameter 'phylogeny' must be a phylogeny"
  )
  expect_error(
    phylo_to_nltts(
      phylogeny = load_tree(model = "mbd", seed = seed),
      mcmc = mcmc,
      n_base_pairs = n_base_pairs,
      seed = "nonsense"
    ),
    "'seed' must be a number"
  )
})
