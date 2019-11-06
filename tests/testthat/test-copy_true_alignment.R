test_that("use", {

  twin_alignment <- copy_true_alignment(get_test_alignment())
  expect_silent(check_alignment(twin_alignment))
})
