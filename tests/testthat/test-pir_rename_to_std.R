test_that("use", {
  pir_params <- create_test_pir_params_setup(
    has_candidate = FALSE,
    has_twinning = FALSE
  )
  folder_name <- tempfile("pir_rename_to_std_1_")
  pir_params <- pir_rename_to_std(
    pir_params,
    folder_name = folder_name
  )

  expect_silent(check_pir_params(pir_params))
  expect_equal(
    pir_params$alignment_params$fasta_filename,
    file.path(folder_name, "alignment.fas")
  )
  expect_equal(
    pir_params$experiments[[1]]$beast2_options$input_filename,
      file.path(folder_name, "gen.xml")
  )
  expect_equal(
    pir_params$experiments[[1]]$beast2_options$output_state_filename,
      file.path(folder_name, "gen.xml.state")
  )
  expect_equal(
    pir_params$experiments[[1]]$errors_filename,
      file.path(folder_name, "errors_gen.csv")
  )
  expect_equal(
    pir_params$experiments[[1]]$inference_model$mcmc$tracelog$filename,
      file.path(folder_name, "gen.log")
  )
  expect_equal(
    pir_params$experiments[[1]]$inference_model$mcmc$screenlog$filename,
      file.path(folder_name, "gen.csv")
  )
  expect_equal(
    pir_params$experiments[[1]]$inference_model$mcmc$treelog$filename,
      file.path(folder_name, "gen.trees")
  )
  expect_equal(
    pir_params$experiments[[1]]$est_evidence_mcmc$tracelog$filename,
      file.path(folder_name, "gen_evidence.log")
  )
  expect_equal(
    pir_params$experiments[[1]]$est_evidence_mcmc$screenlog$filename,
      file.path(folder_name, "gen_evidence.csv")
  )
  expect_equal(
    pir_params$experiments[[1]]$est_evidence_mcmc$treelog$filename,
      file.path(folder_name, "gen_evidence.trees")
  )
})
