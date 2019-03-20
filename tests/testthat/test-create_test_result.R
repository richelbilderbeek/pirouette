context("test-create_test_result")

test_that("use", {
  log_evidence <- -123.456
  weight <- 0.1234
  errors <- c(3, 1, 4, 1, 5, 9, 3)

  result <- create_result(
    log_evidence = log_evidence,
    weight = weight,
    errors = errors
  )

  expect_equal(log_evidence, result$log_evidence)
  expect_equal(weight, result$weight)
  expect_equal(errors, result$errors)
})

test_that("use", {
  expect_silent(check_result(create_test_result()))
})
