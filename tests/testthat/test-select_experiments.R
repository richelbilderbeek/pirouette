context("test-select_experiments")

test_that("use", {

  # Select all experiments with 'run_if' is 'always'
  experiment <- create_experiment()
  experiment$run_if <- "always"
  selected <- select_experiments(experiments)
  expect_equal(1, length(selected))
})
