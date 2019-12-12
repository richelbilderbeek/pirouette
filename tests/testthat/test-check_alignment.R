test_that("use", {
  testthat::expect_silent(pirouette::check_alignment(ape::as.DNAbin("")))
  testthat::expect_error(pirouette::check_alignment(""))
  testthat::expect_error(pirouette::check_alignment(NULL))
  testthat::expect_error(pirouette::check_alignment(NA))
  testthat::expect_error(pirouette::check_alignment(Inf))
})
