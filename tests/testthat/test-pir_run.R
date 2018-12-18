context("pir_run")

test_that("use", {

  true_phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  errors <- pir_run(
    true_phylogeny,
    mcmc = beautier::create_mcmc(chain_length = 2000, store_every = 1000)
  )
  expect_true("tree" %in% names(errors))
  expect_true(is.factor(errors$tree))
  expect_true("true" %in% errors$tree)
  expect_true("twin" %in% errors$tree)

  expect_true("inference_model" %in% names(errors))
  expect_true(is.factor(errors$inference_model))
  expect_true("generative" %in% errors$inference_model)
  expect_true("best" %in% errors$inference_model)

  expect_true("inference_model_weight" %in% names(errors))
  expect_true(!is.factor(errors$inference_model_weight))

  expect_true("site_model" %in% names(errors))
  expect_true(is.factor(errors$site_model))
  expect_true("JC69" %in% errors$site_model)

  expect_true("clock_model" %in% names(errors))
  expect_true(is.factor(errors$clock_model))
  expect_true("strict" %in% errors$clock_model)

  expect_true("tree_prior" %in% names(errors))
  expect_true(is.factor(errors$tree_prior))
  expect_true("BD" %in% errors$tree_prior)

  expect_true("error_1" %in% names(errors))
  expect_true(!is.factor(errors$error_1))
})
