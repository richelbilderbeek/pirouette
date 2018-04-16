context("run")

test_that("use", {

  phylogeny <- ape::read.tree(text = "(((A:1,B:1):1,C:2):1,D:3);")
  out <- run(
    phylogeny = phylogeny,
    sequence_length = 10,
    mutation_rate = 0.1,
    mcmc = beautier::create_mcmc(chain_length = 2000),
    crown_age = 15.0
  )
  testthat::expect_true(class(out$trees) == "multiPhylo")
})
