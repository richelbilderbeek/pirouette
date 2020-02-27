test_that("use", {

  expect_true(
    !will_measure_evidence(
      create_test_pir_params_setup(has_candidate = FALSE)
    )
  )

  if (rappdirs::app_dir()$os == "win") return()

  pir_params <- create_test_pir_params_setup(has_candidate = TRUE)
  expect_true(will_measure_evidence(pir_params))

  pir_params <- create_test_pir_params_setup(has_candidate = FALSE)
  expect_true(!will_measure_evidence(pir_params))

  pir_params$experiments[[1]]$inference_conditions$do_measure_evidence <- TRUE
  pir_params$evidence_filename <- get_temp_evidence_filename()
  expect_true(will_measure_evidence(pir_params))
})
