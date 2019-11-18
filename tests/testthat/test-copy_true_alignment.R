test_that("use", {

  twin_alignment <- pirouette::copy_true_alignment(
    pirouette::get_test_alignment()
  )
  testthat::expect_silent(pirouette::check_alignment(twin_alignment))
})
