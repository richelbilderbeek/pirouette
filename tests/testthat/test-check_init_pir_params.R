test_that("minimal use", {
  pir_params <- create_test_pir_params()
  expect_silent(check_pir_params(pir_params))
  expect_silent(check_init_pir_params(pir_params))
})

test_that("1. detect unintialized first MCMC tracelog", {
  pir_params <- create_test_pir_params()

  # create_test_pir_params also supplies an initialized MCMC.
  # overwrite it by an uninitialized one
  pir_params$experiments[[1]]$inference_model$mcmc$tracelog$filename <-
    create_mcmc()$tracelog$filename

  expect_error(
    check_init_pir_params(pir_params),
    "pir_params.experiments..1...inference_model.mcmc.tracelog.filename is NA"
  )
  pir_params <- init_pir_params(pir_params)
  expect_silent(check_init_pir_params(pir_params))
})

test_that("2. detect unintialized first MCMC treelog", {
  pir_params <- create_test_pir_params()

  # create_test_pir_params also supplies an initialized MCMC.
  # overwrite it by an uninitialized one
  pir_params$experiments[[1]]$inference_model$mcmc$treelog$filename <-
    create_mcmc()$treelog$filename

  # Trigger the second error
  expect_error(
    check_init_pir_params(pir_params),
    "pir_params.experiments..1...inference_model.mcmc.treelog.filename is '..tree..trees" # nolint sorry, long line
  )
  pir_params <- init_pir_params(pir_params)
  expect_silent(check_init_pir_params(pir_params))
})

test_that("3. detect unintialized first MCMC tracelog in evidence MCMC", {
  pir_params <- create_test_pir_params()

  # create_test_pir_params also supplies an initialized MCMC.
  # overwrite it by an uninitialized one
  pir_params$experiments[[1]]$est_evidence_mcmc$tracelog$filename <-
    create_mcmc()$tracelog$filename

  expect_error(
    check_init_pir_params(pir_params),
    "pir_params.experiments..1...est_evidence_mcmc.tracelog.filename is NA"
  )
  pir_params <- init_pir_params(pir_params)
  expect_silent(check_init_pir_params(pir_params))
})

test_that("4. detect unintialized first MCMC treelog", {
  pir_params <- create_test_pir_params()

  # create_test_pir_params also supplies an initialized MCMC.
  # overwrite it by an uninitialized one
  pir_params$experiments[[1]]$est_evidence_mcmc$treelog$filename <-
    create_mcmc()$treelog$filename

  expect_error(
    check_init_pir_params(pir_params),
    "pir_params.experiments..1...est_evidence_mcmc.treelog.filename is '..tree..trees" # nolint sorry, long line
  )
  pir_params <- init_pir_params(pir_params)
  expect_silent(check_init_pir_params(pir_params))
})
