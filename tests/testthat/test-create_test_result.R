context("test-create_test_result")

test_that("use", {
  expect_silent(check_result(create_test_result()))
})
