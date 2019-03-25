context("test-check_experiments")

test_that("use", {

  expect_silent(check_experiments(experiments = list(create_experiment())))
})

test_that("each element in the list must be a proper experiment", {

  experiment_1 <- create_experiment()
  experiment_2 <- "nonsense"
  experiments <- list(experiment_1, experiment_2)
  expect_error(
    check_experiments(experiments)
  )
})

test_that("must have same MCMC chain length", {

  experiment_1 <- create_experiment()
  experiment_2 <- create_experiment()
  experiment_2$inference_model$mcmc$chain_length <-
    experiment_2$inference_model$mcmc$chain_length * 10
  experiments <- list(experiment_1, experiment_2)
  expect_error(
    check_experiments(experiments),
    "All MCMCs in the experiments must be identical"
  )
})

test_that("two experiments, both generative is not allowed", {

  experiment_1 <- create_experiment()
  experiment_2 <- create_experiment()
  experiments <- list(experiment_1, experiment_2)
  expect_error(
    check_experiments(experiments),
    "Specifying more than one 'generative' model experiment is redundant"
  )
})

test_that("two experiments, first candidate, then generative is not allowed", {

  if (!beastier::is_on_travis()) return()

  experiment_1 <- create_experiment(
    inference_conditions = create_inference_conditions(
      model_type = "candidate",
      run_if = "best_candidate",
      do_measure_evidence = "TRUE"
    )
  )
  experiment_2 <- create_experiment()
  experiments <- list(experiment_1, experiment_2)
  expect_error(
    check_experiments(experiments),
    "If multiple experiments, generative is either first or absent"
  )
})

test_that("two experiments, first candidate, then generative is not allowed", {

  if (!beastier::is_on_travis()) return()

  experiments <- create_all_experiments()
  experiment_generative <- create_experiment()
  experiments[[length(experiments) + 1]] <- experiment_generative
  expect_error(
    check_experiments(experiments),
    "If multiple experiments, generative is either first or absent"
  )
})
