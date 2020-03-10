test_that("use", {
  expect_true(is_pir_params(create_test_pir_params()))
  expect_false(is_pir_params("nonsense"))
  expect_false(is_pir_params(NA))
  expect_false(is_pir_params(NULL))
  expect_false(is_pir_params(3.14))
  expect_message(is_pir_params(3.14, verbose = TRUE))
})
