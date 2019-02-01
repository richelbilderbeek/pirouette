context("test-create_standard_mutation_rate")

test_that("use", {
  crown_age <- 15
  expect_equal(
    create_standard_mutation_rate(crown_age),
    1 / 15
  )
})

test_that("abuse", {
  expect_error(
    create_standard_mutation_rate(crown_age = NA)
  )
  expect_error(
    create_standard_mutation_rate(crown_age = -15)
  )
})
