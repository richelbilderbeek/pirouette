test_that("use", {

  site_models <- beautier::create_site_models()
  clock_models <- beautier::create_clock_models()
  tree_priors <- beautier::create_tree_priors()

  inference_models <- combine_models(
    site_models = site_models,
    clock_models = clock_models,
    tree_priors = tree_priors
  )
  expect_equal(
    length(inference_models),
    length(site_models) * length(clock_models) * length(tree_priors)
  )
  for (inference_model in inference_models) {
    check_inference_model(inference_model)
  }
})
