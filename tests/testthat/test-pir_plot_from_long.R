test_that("raw", {
  n <- 1000
  t <- tibble::tibble(
    tree_and_model = rep(get_tree_and_model_values(), each = n),
    error_value = stats::rnorm(4 * n, 0.5, 0.1)
  )
  t$tree_and_model <- forcats::as_factor(t$tree_and_model)
  expect_silent(pir_plot_from_long(t))
  pir_plot_from_long(t)
})

test_that("00", {
  pir_out <- create_test_pir_run_output(
    add_twin = FALSE,
    add_best = FALSE
  )
  t <- convert_pir_out_to_long(pir_out)
  expect_silent(pir_plot_from_long(t))
  pir_plot_from_long(t)
})

test_that("01", {
  pir_out <- create_test_pir_run_output(
    add_twin = FALSE,
    add_best = TRUE
  )
  t <- convert_pir_out_to_long(pir_out)
  expect_silent(pir_plot_from_long(t))
  pir_plot_from_long(t)
})

test_that("10", {
  pir_out <- create_test_pir_run_output(
    add_twin = TRUE,
    add_best = FALSE
  )
  t <- convert_pir_out_to_long(pir_out)
  expect_silent(pir_plot_from_long(t))
  pir_plot_from_long(t)
})

test_that("11", {
  pir_out <- create_test_pir_run_output(
    add_twin = TRUE,
    add_best = TRUE
  )
  t <- convert_pir_out_to_long(pir_out)
  expect_silent(pir_plot_from_long(t))
  pir_plot_from_long(t)
})
