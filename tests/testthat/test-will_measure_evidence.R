test_that("use", {

  pir_params <- create_test_pir_params_setup(has_candidate = TRUE)
  expect_true(will_measure_evidence(pir_params))

  pir_params <- create_test_pir_params_setup(has_candidate = FALSE)
  expect_true(!will_measure_evidence(pir_params))

  pir_params$experiments[[1]]$inference_conditions$do_measure_evidence <- TRUE
  expect_true(will_measure_evidence(pir_params))
})
