test_that("use", {

  # Parameters
  pir_params <- pirouette::create_test_pir_params()
  n_replicates <- 2

  # Replicate pir_params
  testthat::expect_silent(
    pir_paramses <- pirouette::replicate_pir_params(
      n_replicates = n_replicates,
      pir_params = pir_params
    )
  )
  testthat::expect_equal(length(pir_paramses), n_replicates)
})

test_that("abuse", {

  # Parameters
  pir_params <- pirouette::create_test_pir_params()

  # Replicate pir_params
  testthat::expect_error(
    pir_paramses <- pirouette::replicate_pir_params(
      n_replicates = 2.5,
      pir_params = pir_params
    ),
    "'n_replicates' must be an integer"
  )
  testthat::expect_error(
    pir_paramses <- pirouette::replicate_pir_params(
      n_replicates = -4,
      pir_params = pir_params
    ),
    "'n_replicates' must be positive"
  )
})
