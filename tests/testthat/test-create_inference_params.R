context("test-create_inference_params")

test_that("use", {
  expect_equal(2 * 2, 4)
})

test_that("abuse", {

  expect_error(
    create_inference_params(
      mcmc = beautier::create_mcmc(chain_length = 2000),
      rng_seed = -123456789
    ),
    "'rng_seed' should be NA or non-zero positive"
  )
})

