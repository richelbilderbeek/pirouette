context("test-select_experiments")

test_that("generative", {

  # Select all experiments with 'run_if' is 'always'
  experiment <- create_experiment()
  experiment$run_if <- "always"
  experiments <- list(experiment)


  selected <- select_experiments(
    experiments = experiments
  )
  expect_equal(1, length(selected))
})

test_that("most_evidence", {

  marg_liks <- create_test_marg_liks(
    site_models = list(create_jc69_site_model()),
    clock_models = list(create_strict_clock_model()),
    tree_priors = list(create_yule_tree_prior(), create_bd_tree_prior())
  )
  marg_liks$weight <- c(0.9, 0.1) # in favor of Yule

  experiment_yule <- create_experiment(
    run_if = "best_candidate",
    inference_model = create_inference_model(
      tree_prior = create_yule_tree_prior()
    )
  )
  experiment_bd <- create_experiment(
    run_if = "best_candidate",
    inference_model = create_inference_model(
      tree_prior = create_bd_tree_prior()
    )
  )
  experiments <- list(experiment_yule, experiment_bd)
  inference_models <- select_experiments(
    experiments = experiments,
    marg_liks = marg_liks
  )

  expect_equal(1, length(inference_models))
  expect_equal("yule", inference_models[[1]]$inference_model$tree_prior$name)
})
