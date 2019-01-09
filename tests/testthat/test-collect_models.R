context("test-collect_models")

test_that("generative model selection creates one model combination", {

  # Pick the generative model
  alignment_params <- create_alignment_params(
    root_sequence = "acgt", mutation_rate = 0.01
  )
  model_select_params <- create_gen_model_select_params(alignment_params)
  names(model_select_params)
  length(model_select_params$tree_priors)

  models <- collect_models(model_select_params)
  expect_equal(1, length(models))
  model <- models[[1]]
  expect_true("site_model" %in% names(model))
  expect_true("clock_model" %in% names(model))
  expect_true("tree_prior" %in% names(model))

  testthat::expect_equal(
    alignment_params$site_model,
    model$site_model
  )
  testthat::expect_equal(
    alignment_params$clock_model,
    model$clock_model
  )
  # By default, a birth-death model is assumed to underly the phylogeny
  testthat::expect_equal(
    model$tree_prior,
    beautier::create_bd_tree_prior()
  )

})

test_that("most-evidence model selection creates all combinations", {

  # Pick the model with most evidence to be used in inference
  model_select_params <- create_most_evidence_model_select_params()

  models <- collect_models(model_select_params)

  expect_true(length(models) >= 8)
})
