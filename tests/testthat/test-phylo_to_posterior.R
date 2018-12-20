context("test-phylo_to_posterior")

test_that("use", {

  if (!beastier::is_on_ci()) return()

  phylogeny <- ape::read.tree(text = "(((A:1,B:1):1,C:2):1,D:3);")
  out <- phylo_to_posterior(
    phylogeny = phylogeny,
    alignment_params = create_alignment_params(
      root_sequence = create_blocked_dna(length = 8),
      mutation_rate = 0.1
    ),
    mcmc = beautier::create_mcmc(chain_length = 2000),
    crown_age = 15.0
  )
  testthat::expect_true(class(out$trees) == "multiPhylo")
})

test_that("abuse", {

  phylogeny <- ape::read.tree(text = "(((A:1,B:1):1,C:2):1,D:3);")
  alignment_params <- create_alignment_params(
    root_sequence = create_blocked_dna(length = 4),
    mutation_rate = 0.1
  )

  expect_error(
    phylo_to_posterior(
      phylogeny = phylogeny,
      alignment_params = alignment_params,
      mcmc = beautier::create_mcmc(chain_length = 2000),
      crown_age = 15.0,
      beast2_rng_seed = -123456789

    ),
    "'beast2_rng_seed' should be NA or non-zero positive"
  )
  expect_error(
    phylo_to_posterior(
      phylogeny = phylogeny,
      alignment_params = "nonsense",
      mcmc = beautier::create_mcmc(chain_length = 2000)
    ),
    "'alignment_params' must be a set of alignment parameters"
  )
})
