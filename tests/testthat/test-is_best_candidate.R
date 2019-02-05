context("test-is_best_candidate")

test_that("use", {

  marg_liks <- create_test_marg_liks(
    site_models = list(create_jc69_site_model()),
    clock_models = list(create_strict_clock_model()),
    tree_priors = list(create_yule_tree_prior(), create_bd_tree_prior())
  )
  marg_liks$weight <- c(0.9, 0.1) # in favor of Yule

  experiment_yule <- create_experiment(
    inference_model = create_inference_model(
      tree_prior = create_yule_tree_prior()
    )
  )
  experiment_bd <- create_experiment(
    inference_model = create_inference_model(
      tree_prior = create_bd_tree_prior()
    )
  )
  expect_true(is_best_candidate(experiment_yule, marg_liks))
  skip("WIP, Issue 69, #69")
  expect_false(is_best_candidate(experiment_bd, marg_liks))
})
