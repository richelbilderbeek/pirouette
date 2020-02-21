test_that("use", {

  pir_params <- create_test_pir_params()

  # create_test_pir_params also supplies an initialized MCMC.
  # overwrite it by an uninitialized one
  pir_params$experiments[[1]]$inference_model$mcmc <- create_mcmc()

  experiment <- pir_params$experiments[[1]]
  alignment_params <- pir_params$alignment_params
  alignment_folder <- dirname(alignment_params$fasta_filename)

  expect_true(
    is.na(
      experiment$inference_model$mcmc$tracelog$filename
    )
  )
  expect_equal(
    experiment$inference_model$mcmc$treelog$filename,
    "$(tree).trees"
  )

  experiment <- init_experiment(
    experiment = experiment,
    alignment_params = alignment_params
  )

  expected_tracelog_filename <- file.path(
    alignment_folder,
      paste0(
      beautier::get_alignment_id(alignment_params$fasta_filename),
      ".log"
    )
  )
  expected_treelog_filename <- file.path(
    alignment_folder,
    paste0(
      beautier::get_alignment_id(alignment_params$fasta_filename),
      ".trees"
    )
  )
  expect_equal(
    experiment$inference_model$mcmc$tracelog$filename,
    expected_tracelog_filename
  )
  expect_equal(
    experiment$inference_model$mcmc$treelog$filename,
    expected_treelog_filename
  )
})


test_that("use, keep path of treelog filename", {

  skip("WIP")
  pir_params <- create_test_pir_params()

  # create_test_pir_params also supplies an initialized MCMC.
  # overwrite it by an uninitialized one
  pir_params$experiments[[1]]$inference_model$mcmc <- create_mcmc()

  experiment <- pir_params$experiments[[1]]
  alignment_params <- pir_params$alignment_params
  alignment_folder <- dirname(alignment_params$fasta_filename)

  expect_true(
    is.na(
      experiment$inference_model$mcmc$tracelog$filename
    )
  )
  expect_equal(
    experiment$inference_model$mcmc$treelog$filename,
    "$(tree).trees"
  )

  experiment <- init_experiment(
    experiment = experiment,
    alignment_params = alignment_params
  )

  expected_tracelog_filename <- file.path(
    alignment_folder,
      paste0(
      beautier::get_alignment_id(alignment_params$fasta_filename),
      ".log"
    )
  )
  expected_treelog_filename <- file.path(
    alignment_folder,
    paste0(
      beautier::get_alignment_id(alignment_params$fasta_filename),
      ".trees"
    )
  )
  expect_equal(
    experiment$inference_model$mcmc$tracelog$filename,
    expected_tracelog_filename
  )
  expect_equal(
    experiment$inference_model$mcmc$treelog$filename,
    expected_treelog_filename
  )
})
