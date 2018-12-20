context("test-phylo_to_posterior")

test_that("use", {

  if (!beastier::is_on_ci()) return()

  phylogeny <- ape::read.tree(text = "(((A:1,B:1):1,C:2):1,D:3);")
  out <- phylo_to_posterior(
    phylogeny = phylogeny,
    root_sequence = create_blocked_dna(length = 8),
    mutation_rate = 0.1,
    mcmc = beautier::create_mcmc(chain_length = 2000),
    crown_age = 15.0
  )
  testthat::expect_true(class(out$trees) == "multiPhylo")
})

test_that("abuse", {

  phylogeny <- ape::read.tree(text = "(((A:1,B:1):1,C:2):1,D:3);")

  expect_error(
    phylo_to_posterior(
      phylogeny = phylogeny,
      root_sequence = create_blocked_dna(length = 4),
      mutation_rate = 0.1,
      mcmc = beautier::create_mcmc(chain_length = 2000),
      crown_age = 15.0,
      beast2_rng_seed = -123456789

    ),
    "'beast2_rng_seed' should be NA or non-zero positive"
  )
  expect_error(
    phylo_to_posterior(
      phylogeny = phylogeny,
      root_sequence = "nonsense",
      mutation_rate = 0.1,
      mcmc = beautier::create_mcmc(chain_length = 2000)
    ),
    "'root_sequence' should be a lower-case DNA character string"
  )
})
