test_that("use", {

  pir_out <- create_test_pir_run_output(
    add_twin = TRUE,
    add_best = TRUE
  )
  pir_plot(pir_out)

  expect_equal(2 * 2, 4)
})
