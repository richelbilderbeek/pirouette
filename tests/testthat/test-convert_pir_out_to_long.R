test_that("use", {
  pir_out <- create_test_pir_run_output(
    add_twin = TRUE,
    add_best = TRUE
  )
  t <- convert_pir_out_to_long(pir_out)
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
  pir_plot_from_long(df_long = t)
})
