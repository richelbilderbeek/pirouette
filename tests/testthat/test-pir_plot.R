context("test-pir_plot")

test_that("use", {

  expect_silent(
    pir_plot(
      create_pir_run_test_output(
        add_twin = FALSE,
        add_best = FALSE
      )
    )
  )

  expect_silent(
    pir_plot(
      create_pir_run_test_output(
        add_twin = FALSE,
        add_best = TRUE
      )
    )
  )
  expect_silent(
    pir_plot(
      create_pir_run_test_output(
        add_twin = TRUE,
        add_best = FALSE
      )
    )
  )
  expect_silent(
    pir_plot(
      pir_out = create_pir_run_test_output(
        add_twin = TRUE,
        add_best = TRUE
      )
    )
  )

})
