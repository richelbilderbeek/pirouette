test_that("use", {
  expect_true(
    !is.na(
      stringr::str_match(
        string = get_temp_errors_filename(),
        "errors_.*\\.csv"
<<<<<<< HEAD
      )[1,1]
=======
      )[1, 1]
>>>>>>> b31a67ccf7a115ac420237774dfccbe724a0a7fa
    )
  )
})
