test_that("00", {
  pir_out <- create_test_pir_run_output(
    add_twin = FALSE,
    add_best = FALSE
  )
  expect_silent(get_pir_plot_tree_and_model_descriptions(pir_out))
})
test_that("01", {
  pir_out <- create_test_pir_run_output(
    add_twin = FALSE,
    add_best = TRUE
  )
  expect_silent(get_pir_plot_tree_and_model_descriptions(pir_out))
})
test_that("10", {
  pir_out <- create_test_pir_run_output(
    add_twin = TRUE,
    add_best = FALSE
  )
  expect_silent(get_pir_plot_tree_and_model_descriptions(pir_out))
})
test_that("11", {
  pir_out <- create_test_pir_run_output(
    add_twin = TRUE,
    add_best = TRUE
  )
  expect_silent(get_pir_plot_tree_and_model_descriptions(pir_out))
})
