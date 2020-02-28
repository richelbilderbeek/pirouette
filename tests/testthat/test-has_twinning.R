test_that("no twinning", {
  pir_params <- create_test_pir_params()
  expect_false(has_twinning(pir_params))
})

test_that("twinning", {

  pir_params <- create_test_pir_params(
    twinning_params = create_twinning_params()
  )
  expect_true(has_twinning(pir_params))
})
