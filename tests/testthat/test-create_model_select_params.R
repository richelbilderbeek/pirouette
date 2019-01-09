context("test-create_model_select_params")

test_that("use", {

  model_select_params <- create_model_select_params()
  expect_true("site_models" %in% names(model_select_params))
  expect_true("clock_models" %in% names(model_select_params))
  expect_true("tree_priors" %in% names(model_select_params))
})
