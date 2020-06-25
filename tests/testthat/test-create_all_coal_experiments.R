test_that("use", {

  if (rappdirs::app_dir()$os == "win")  return()
  if (!beastier::is_on_ci()) return()

  experiments <- create_all_coal_experiments()

  pirouette::check_experiments(experiments)

  tree_prior_names <- unlist(
    lapply(experiments, function(x) x$inference_model$tree_prior$name)
  )

  expect_true(
    all(
      tree_prior_names %in% c(
        "coalescent_bayesian_skyline",
        "coalescent_constant_population",
        "coalescent_exp_population"
      )
    )
  )
})

test_that("abuse", {

  if (rappdirs::app_dir()$os == "win")  return()

  # Yule is not a coalescent prior
  expect_error(
    create_all_coal_experiments(
      tree_priors = list(beautier::create_yule_tree_prior())
    )
  )
  # BD is not a coalescent prior
  expect_error(
    create_all_coal_experiments(
      tree_priors = list(beautier::create_bd_tree_prior())
    )
  )
})
