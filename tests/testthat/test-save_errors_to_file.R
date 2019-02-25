context("test-save_errors_to_file")

test_that("df incorrect", {
  expect_error(
    save_errors_to_file(
      ape::rcoal(4),
      pir_params = create_test_pir_params()
    ),
    "Saving errors to file failed"
  )
})
