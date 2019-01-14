context("test-create_model_select_param")

test_that("use", {

  model_select_param <- create_model_select_param(type = "generative")
  expect_true("site_models" %in% names(model_select_param))
  expect_true("clock_models" %in% names(model_select_param))
  expect_true("tree_priors" %in% names(model_select_param))
})
