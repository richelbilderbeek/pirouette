context("test-utils")

test_that("get_twin_models", {
  expect_true(
    length(get_twin_models()) > 0 # nolint internal function
  )
  expect_true(
    is.character(get_twin_models()) # nolint internal function
  )
})

test_that("get_twin_methods", {
  expect_true(
    length(get_twin_methods()) > 0 # nolint internal function
  )
  expect_true(
    is.character(get_twin_methods()) # nolint internal function
  )
})
