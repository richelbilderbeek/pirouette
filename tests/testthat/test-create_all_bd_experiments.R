test_that("use", {

  if (rappdirs::app_dir()$os == "win")  return()

  experiments <- create_all_bd_experiments()

  pirouette::check_experiments(experiments)


  tree_prior_names <- unlist(
    lapply(experiments, function(x) x$inference_model$tree_prior$name)
  )
  expect_true(all(tree_priors %in% c("yule", "birth_death")))
})

test_that("abuse", {

  if (rappdirs::app_dir()$os == "win")  return()

  # Coalescent Bayesian Skyline is not a Birth-Death model
  expect_error(
    create_all_bd_experiments(
      tree_priors = list(beautier::create_cbs_tree_prior())
    )
  )
  # Coalescent Constant Population is not a Birth-Death model
  expect_error(
    create_all_bd_experiments(
      tree_priors = list(beautier::create_ccp_tree_prior())
    )
  )
  # Coalescent Exponential Population is not a Birth-Death model
  expect_error(
    create_all_bd_experiments(
      tree_priors = list(beautier::create_cep_tree_prior())
    )
  )
})
