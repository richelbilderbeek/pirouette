context("test-check_run_experiment")

test_that("use", {
  expect_silent(
    check_run_experiment(
      create_test_run_experiment()
    )
  )
})
