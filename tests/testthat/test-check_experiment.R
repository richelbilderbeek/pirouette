context("test-check_experiment")

test_that("use", {

  expect_silent(check_experiment(create_experiment()))
})
