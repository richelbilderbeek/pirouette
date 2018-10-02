context("pir_run")

test_that("use", {

  phylogeny <- ape::read.tree(text = "(((A:1,B:1):1,C:2):1,D:3);")
  out <- pir_run(
    phylogeny = phylogeny,
    sequence_length = 10,
    root_sequence = "aaaaaaaaaa",
    mutation_rate = 0.1,
    mcmc = beautier::create_mcmc(chain_length = 2000),
    crown_age = 15.0
  )
  testthat::expect_true(class(out$trees) == "multiPhylo")
})

test_that("use with MRCA distribution", {

  phylogeny <- ape::read.tree(text = "(((A:1,B:1):1,C:2):1,D:3);")
  testthat::expect_silent(
    pir_run(
      phylogeny = phylogeny,
      sequence_length = 10,
      mutation_rate = 0.1,
      mcmc = beautier::create_mcmc(chain_length = 2000),
      crown_age = 15.0,
      mrca_distr = beautier::create_uniform_distr(upper = 16)
    )
  )
})

test_that("abuse", {

  phylogeny <- ape::read.tree(text = "(((A:1,B:1):1,C:2):1,D:3);")


  expect_error(
    pir_run(
      phylogeny = phylogeny,
      sequence_length = 10,
      root_sequence = "a", # too short
      mutation_rate = 0.1,
      mcmc = beautier::create_mcmc(chain_length = 2000),
      crown_age = 15.0

    ),
    paste0(
      "'sequence_length' must be NULL ",
      "or equal the number of characters in 'root_sequence'"
    )
  )
  expect_error(
    pir_run(
      phylogeny = phylogeny,
      sequence_length = 10,
      mutation_rate = 0.1,
      mcmc = beautier::create_mcmc(chain_length = 2000),
      crown_age = 15.0,
      beast2_rng_seed = -123456789

    ),
    "'beast2_rng_seed' should be NA or non-zero positive"
  )
  expect_error(
    pir_run(
      phylogeny = phylogeny,
      sequence_length = 10,
      root_sequence = "nonsense",
      mutation_rate = 0.1,
      mcmc = beautier::create_mcmc(chain_length = 2000)
    ),
    "'root_sequence' should be a lower-case DNA character string"
  )
})
