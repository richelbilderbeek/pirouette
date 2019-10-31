test_that("use", {

  pir_params <- create_test_pir_params()
  experiment <- pir_params$experiments[[1]]
  alignment_params <- pir_params$alignment_params

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

  expected_tracelog_filename <- paste0(
    beautier::get_alignment_id(alignment_params$fasta_filename),
    ".log"
  )
  expected_treelog_filename <- paste0(
    beautier::get_alignment_id(alignment_params$fasta_filename),
    ".trees"
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
