context("test-select_experiments")

test_that("use, always", {
  # Select all experiments with 'run_if' is 'always'
  experiment <- create_experiment()
  experiment$run_if <- "always"
  experiments <- list(experiment)
  selected <- select_experiments(experiments)
  expect_equal(1, length(selected))
})

test_that("use, most_evidence", {
  skip("WIP, Issue 69, #69")
  # Select all experiments with 'run_if' is 'always'
  experiment_1 <- create_experiment()
  experiment_2 <- create_experiment()
  experiment_3 <- create_experiment()
  experiment_1$run_if <- "best_candidate"
  experiment_2$run_if <- "best_candidate"
  experiment_3$run_if <- "best_candidate"
  experiments <- list(experiment_1, experiment_2, experiment_3)
  marg_liks <- data.frame(weights = c(0.1, 0.5, 0.4))
  selected <- select_experiments(experiments, marg_liks)
  expect_equal(1, length(selected))
})
