context("test-check_experiments")

test_that("use", {

  expect_silent(check_experiments(experiments = list(create_experiment())))
})
