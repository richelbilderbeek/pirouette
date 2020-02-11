context("test-pir_plot")

test_that("use", {

  suppressPackageStartupMessages(library(ggplot2))

  x <- utils::capture.output(
    pirouette::pir_plot(
      pir_out = create_test_pir_run_output(
        add_twin = FALSE,
        add_best = FALSE
      )
    )
  )
  rm(x)

  testthat::expect_silent(
    pirouette::pir_plot(
      pir_out = create_test_pir_run_output(
        add_twin = FALSE,
        add_best = FALSE
      )
    )
  )
  testthat::expect_silent(
    pirouette::pir_plot(
      pir_out = create_test_pir_run_output(
        add_twin = FALSE,
        add_best = TRUE
      )
    )
  )
  testthat::expect_silent(
    pirouette::pir_plot(
      create_test_pir_run_output(
        add_twin = TRUE,
        add_best = FALSE
      )
    )
  )
  testthat::expect_silent(
    pirouette::pir_plot(
      pir_out = create_test_pir_run_output(
        add_twin = TRUE,
        add_best = TRUE
      )
    )
  )

})
