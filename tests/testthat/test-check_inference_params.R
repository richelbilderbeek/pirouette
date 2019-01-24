context("test-check_inference_params")

test_that("abuse", {
  inference_params <- create_inference_params(
    mcmc = beautier::create_mcmc(chain_length = 2000),
    rng_seed = 1
  )
  inference_param2 <- list()
  inference_param2$mrca_prior <- inference_params$mrca_prior
  inference_param2$mcmc <- inference_params$mcmc
  inference_param2$rng_seed <- inference_params$rng_seed
  # inference_params$beast2_path is missing
  inference_param2$verbose <- inference_params$verbose
  expect_error(
    check_inference_params(inference_param2),
    "'beast2_path' must be an element of an 'inference_params'"
  )

  inference_param3 <- inference_params
  inference_param3$rng_seed <- -10
  expect_error(
    check_inference_params(inference_param3),
    "'rng_seed' should be NA or non-zero positive"
  )
})
