test_that("use", {

  pir_params <- create_test_pir_params()

  # create_test_pir_params also supplies an initialized MCMC.
  # overwrite it by an uninitialized one
  pir_params$experiments[[1]]$inference_model$mcmc <- create_mcmc()

  expect_true(
    is.na(
      pir_params$experiments[[1]]$inference_model$mcmc$tracelog$filename
    )
  )
  expect_equal(
    pir_params$experiments[[1]]$inference_model$mcmc$treelog$filename,
    "$(tree).trees"
  )

  pir_params <- init_pir_params(pir_params)

  expected_tracelog_filename <- paste0(
    beautier::get_alignment_id(pir_params$alignment_params$fasta_filename),
    ".log"
  )
  expected_treelog_filename <- paste0(
    beautier::get_alignment_id(pir_params$alignment_params$fasta_filename),
    ".trees"
  )

  expect_equal(
    pir_params$experiments[[1]]$inference_model$mcmc$tracelog$filename,
    expected_tracelog_filename
  )
  expect_equal(
    pir_params$experiments[[1]]$inference_model$mcmc$treelog$filename,
    expected_treelog_filename
  )
})
