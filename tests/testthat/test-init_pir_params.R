test_that("use", {

  pir_params <- create_test_pir_params()

  # create_test_pir_params also supplies an initialized MCMC.
  # overwrite it by an uninitialized one
  pir_params$experiments[[1]]$inference_model$mcmc <- beautier::create_mcmc()
  alignment_folder <- dirname(pir_params$alignment_params$fasta_filename)

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

  expected_tracelog_filename <- file.path(
    alignment_folder,
    paste0(
      beautier::get_alignment_id(pir_params$alignment_params$fasta_filename),
      ".log"
    )
  )
  expected_treelog_filename <- file.path(
    alignment_folder,
    paste0(
      beautier::get_alignment_id(pir_params$alignment_params$fasta_filename),
      ".trees"
    )
  )

  for (i in seq_along(pir_params$experiments)) {
    expect_equal(
      pir_params$experiments[[i]]$inference_model$mcmc$tracelog$filename,
      expected_tracelog_filename
    )
    expect_equal(
      pir_params$experiments[[i]]$inference_model$mcmc$treelog$filename,
      expected_treelog_filename
    )
    expect_true(
      pir_params$experiments[[i]]$est_evidence_mcmc$treelog$filename !=
      "$(tree).trees"
    )
    expect_true(
      !is.na(pir_params$experiments[[i]]$est_evidence_mcmc$tracelog$filename)
    )
  }
})
