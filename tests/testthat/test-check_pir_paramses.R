test_that("use", {
  pir_paramses <- list(create_test_pir_params(), create_test_pir_params())
  expect_silent(check_pir_paramses(pir_paramses))
})

test_that("abuse", {
  expect_error(check_pir_paramses("nonsense"), "'pir_paramses' must be a list")
  expect_error(check_pir_paramses(NA), "'pir_paramses' must be a list")
  expect_error(check_pir_paramses(NULL), "'pir_paramses' must be a list")
  expect_error(
    check_pir_paramses(create_test_pir_params()),
    paste0(
      "'pir_paramses' must be a list of 'pir_params'. ",
      "Actual value is a 'pir_params'"
    )
  )

  pir_paramses <- list(create_test_pir_params(), create_test_pir_params())
  pir_paramses[[1]] <- "nonsense"
  expect_error(
    check_pir_paramses(pir_paramses),
    "Element #1 is not a valid pir_params"
  )

  pir_paramses <- list(create_test_pir_params(), create_test_pir_params())
  pir_paramses[[2]] <- "nonsense"
  expect_error(
    check_pir_paramses(pir_paramses),
    "Element #2 is not a valid pir_params"
  )
})
