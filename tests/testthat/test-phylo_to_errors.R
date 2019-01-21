context("test-phylo_to_errors")

test_that("use", {

  skip("For Richel: the check on fasta file existence will always fail")

  n_base_pairs <- 4
  seed <- 1
  phylogeny <- load_tree(tree_model = "mbd", seed = seed)

  nltts <- phylo_to_errors(
    phylogeny = phylogeny,
    alignment_params = create_alignment_params(
      root_sequence = "acgt",
      mutation_rate = 0,
      site_model = beautier::create_jc69_site_model(),
      clock_model = beautier::create_strict_clock_model(),
      rng_seed = 0
    ),
    inference_model = create_inference_model(
      site_model = beautier::create_jc69_site_model(),
      clock_model = beautier::create_strict_clock_model(),
      tree_prior = beautier::create_tree_prior_bd()
    ),
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
