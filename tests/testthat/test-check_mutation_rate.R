test_that("minimal use", {

  expect_silent(check_mutation_rate(0.01))
  expect_silent(check_mutation_rate(1.23))
  expect_silent(
    check_mutation_rate(
      function(phylogeny) {
        1.0 / beautier::get_crown_age(phylogeny)
      }
    )
  )

  expect_error(check_mutation_rate(0.0))
  expect_error(check_mutation_rate(-1.23))
  expect_error(check_mutation_rate(""))
  expect_error(check_mutation_rate(NULL))
  expect_error(check_mutation_rate(NA))
  expect_error(check_mutation_rate(Inf))
  expect_error(check_mutation_rate(c(0.1, 0.2)))
})


test_that("minimal use", {

  expect_error(
    check_mutation_rate(-123.456),
    "'mutation_rate' must be one non-zero and finite positive value"
  )

  expect_error(
    check_mutation_rate(
      function(phylogeny) "nonsense"
    ),
    "'mutation_rate' function must return a number"
  )

  expect_error(
    check_mutation_rate(
      function(phylogeny) -1234567
    ),
    "'mutation_rate' function must return non-zero and positive value"
  )
})
