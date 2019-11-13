test_that("use", {
  expect_true(
    !is.na(
      stringr::str_match(
        string = get_temp_errors_filename(),
        "errors_.*\\.csv"
      )[1, 1]
    )
  )
})
