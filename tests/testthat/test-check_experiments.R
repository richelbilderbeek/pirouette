context("test-check_experiments")

test_that("use", {

  expect_silent(check_experiments(experiments = list(create_experiment())))
})

test_that("must have same MCMC chain length", {

  experiment_1 <- create_experiment()
  experiment_2 <- create_experiment()
  experiment_2$inference_model$mcmc$chain_length <-
    experiment_2$inference_model$mcmc$chain_length * 10
  expect_error(
    check_experiments(experiments),
    "experiments must have same MCMCs"
  )
})
