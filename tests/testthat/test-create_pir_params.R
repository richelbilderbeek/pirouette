test_that("use", {

  alignment_params <- create_test_alignment_params()
  twinning_params <- create_twinning_params()
  experiments <- list(create_test_experiment())
  error_measure_params <- create_error_measure_params()
  evidence_filename <- NA
  verbose <- FALSE

  pir_params <- create_pir_params(
    alignment_params = alignment_params,
    twinning_params = twinning_params,
    experiments = experiments,
    error_measure_params = error_measure_params,
    evidence_filename = evidence_filename,
    verbose = verbose
  )

  expect_equal(alignment_params, pir_params$alignment_params)
  expect_equal(twinning_params, pir_params$twinning_params)
  expect_equal(experiments, pir_params$experiments)
  expect_equal(error_measure_params, pir_params$error_measure_params)
  expect_equal(evidence_filename, pir_params$evidence_filename)
  expect_equal(verbose, pir_params$verbose)

})
