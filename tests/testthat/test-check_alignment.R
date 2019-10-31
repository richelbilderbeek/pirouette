test_that("use", {
  expect_silent(check_alignment(ape::as.DNAbin("")))
  expect_error(check_alignment(""))
  expect_error(check_alignment(NULL))
  expect_error(check_alignment(NA))
  expect_error(check_alignment(Inf))
})
