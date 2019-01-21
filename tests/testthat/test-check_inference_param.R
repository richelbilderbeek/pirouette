context("test-check_inference_param")

test_that("abuse", {
  inference_param <- create_inference_param(
    mcmc = beautier::create_mcmc(chain_length = 2000),
    rng_seed = 1
  )
  inference_param2 <- list()
  inference_param2$mrca_prior <- inference_param$mrca_prior
  inference_param2$mcmc <- inference_param$mcmc
  inference_param2$rng_seed <- inference_param$rng_seed
  # inference_param$beast2_path is missing
  inference_param2$verbose <- inference_param$verbose
  expect_error(
    check_inference_param(inference_param2),
    "'beast2_path' must be an element of an 'inference_param'. Tip: use 'create_inference_param'" # nolint long string
  )

  inference_param3 <- inference_param
  inference_param3$rng_seed <- -10
  expect_error(
    check_inference_param(inference_param3),
    "'rng_seed' should be NA or non-zero positive"
  )
})
