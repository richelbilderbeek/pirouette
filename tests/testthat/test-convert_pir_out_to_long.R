test_that("use", {
  pir_out <- create_test_pir_run_output(
    add_twin = TRUE,
    add_best = TRUE
  )
  t <- convert_pir_out_to_long(pir_out)
  expect_true("tree" %in% names(t))
  expect_true("inference_model" %in% names(t))
  expect_true("inference_model_weight" %in% names(t))
  expect_true("site_model" %in% names(t))
  expect_true("clock_model" %in% names(t))
  expect_true("tree_prior" %in% names(t))
  expect_true("error_index" %in% names(t))
  expect_true("error_value" %in% names(t))
  expect_true("tree_and_model" %in% names(t))
  expect_true("model_setting"  %in% names(t))
  expect_silent(pir_plot_from_long(t))
})

test_that("strip", {
  pir_out <- create_test_pir_run_output(
    add_twin = TRUE,
    add_best = TRUE
  )
  t <- convert_pir_out_to_long(pir_out)
  t <- subset(t, select = -tree)
  t <- subset(t, select = -inference_model_weight)
  t <- subset(t, select = -site_model)
  t <- subset(t, select = -clock_model)
  t <- subset(t, select = -tree_prior)
  pir_plot_from_long(t)
})
