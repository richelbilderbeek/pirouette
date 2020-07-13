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
