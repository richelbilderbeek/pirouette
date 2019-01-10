context("pir_run")

test_that("generative only", {

  if (!beastier::is_on_travis()) return()

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- create_alignment_params(
    mutation_rate = 0.01
  )
  errors <- pir_run(
    phylogeny = phylogeny,
    alignment_params = alignment_params,
    model_select_params = list(
      create_gen_model_select_param(
        alignment_params = alignment_params
      )
    ),
    inference_param = create_inference_param(
      mcmc = beautier::create_mcmc(chain_length = 2000, store_every = 1000)
    )
  )
  expect_true("tree" %in% names(errors))
  expect_true(is.factor(errors$tree))
  expect_true("true" %in% errors$tree)

  expect_true("inference_model" %in% names(errors))
  expect_true(is.factor(errors$inference_model))
  expect_true("generative" %in% errors$inference_model)

  expect_true("inference_model_weight" %in% names(errors))
  expect_true(is.na(errors$inference_model_weight))
  expect_true(!is.factor(errors$inference_model_weight))

  expect_true("site_model" %in% names(errors))
  expect_true(is.factor(errors$site_model))
  expect_true("JC69" %in% errors$site_model)

  expect_true("clock_model" %in% names(errors))
  expect_true(is.factor(errors$clock_model))
  expect_true("strict" %in% errors$clock_model)

  expect_true("tree_prior" %in% names(errors))
  expect_true(is.factor(errors$tree_prior))
  expect_true("birth_death" %in% errors$tree_prior)

  expect_true("error_1" %in% names(errors))
  expect_true(!is.factor(errors$error_1))
})

test_that("most_evidence", {

  if (!beastier::is_on_travis()) return()

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  errors <- pir_run(
    phylogeny = phylogeny,
    alignment_params = create_alignment_params(
      root_sequence = "acgt",
      mutation_rate = 0.01
    ),
    model_select_params = list(
      create_best_model_select_param(
        site_models = beautier::create_site_models()[1],
        clock_models = beautier::create_clock_models()[1],
        tree_priors = beautier::create_tree_priors()[1]
      )
    ),
    inference_param = create_inference_param(
      mcmc = beautier::create_mcmc(chain_length = 2000, store_every = 1000)
    )
  )
  expect_true("most_evidence" %in% errors$inference_model)
  expect_true(all(errors$inference_model_weight > 0.0))
})
