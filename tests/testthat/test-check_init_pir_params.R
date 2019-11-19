test_that("use", {
  pir_params <- create_test_pir_params()

  # create_test_pir_params also supplies an initialized MCMC.
  # overwrite it by an uninitialized one
  pir_params$experiments[[1]]$inference_model$mcmc <- create_mcmc()

  expect_error(
    check_init_pir_params(pir_params),
    "pir_params.experiments..1...inference_model.mcmc.tracelog.filename is NA"
  )
  pir_params <- init_pir_params(pir_params)
  expect_silent(check_init_pir_params(pir_params))
})

test_that("use", {
  pir_params <- create_test_pir_params()

  # create_test_pir_params also supplies an initialized MCMC.
  # overwrite it by an uninitialized one
  pir_params$experiments[[1]]$inference_model$mcmc <- create_mcmc()

  # Prevent the first error from happening
  pir_params$experiments[[1]]$inference_model$mcmc$tracelog$filename <-
    paste0(
      beautier::get_alignment_id(pir_params$alignment_params$fasta_filename),
      ".log"
    )

  # Trigger the second error
  expect_error(
    check_init_pir_params(pir_params),
    "pir_params.experiments..1...inference_model.mcmc.treelog.filename is '..tree..trees" # nolint sorry, long line
  )
  pir_params <- init_pir_params(pir_params)
  expect_silent(check_init_pir_params(pir_params))
})
