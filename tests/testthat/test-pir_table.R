context("test-pir_table")

test_that("use", {
  expect_silent(
    pir_table(
      run_experiments = list(create_test_run_experiment())
    )
  )
})
