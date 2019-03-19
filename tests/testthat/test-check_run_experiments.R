context("test-check_run_run_experiments")

test_that("use", {
  expect_silent(
    check_run_experiments(
      run_experiments = list(create_test_run_experiment())
    )
  )
})

test_that("each element in the list must be a proper experiment", {

  experiment_1 <- create_test_run_experiment()
  experiment_2 <- "nonsense"
  experiments <- list(experiment_1, experiment_2)
  expect_error(
    check_run_experiments(experiments)
  )
})
