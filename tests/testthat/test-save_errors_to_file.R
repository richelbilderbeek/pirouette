context("test-save_errors_to_file")

test_that("df incorrect", {
  pir_params <- create_test_pir_params()
  save_errors_to_file(ape::rcoal(4), pir_params = pir_params)
})
