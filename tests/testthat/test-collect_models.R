context("test-collect_models")

test_that("generative model selection creates one model combination", {

  # Pick the generative model
  alignment_params <- create_alignment_params(
    root_sequence = "acgt", mutation_rate = 0.01
  )
  model_select_params <- create_gen_model_select_param(alignment_params)

  # Check its content
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
  model_select_params <- create_best_model_select_param()

  models <- collect_models(model_select_params)

  n_site_models <- length(beautier::create_site_models())
  n_clock_models <- length(beautier::create_clock_models())
  n_tree_priors <- length(beautier::create_tree_priors())
  n_models <- n_site_models * n_clock_models * n_tree_priors

  expect_equal(length(models), n_models)
})
