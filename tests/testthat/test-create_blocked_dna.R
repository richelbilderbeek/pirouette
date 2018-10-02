context("create_blocked_dna")

test_that("use", {
  expect_equal(
    create_blocked_dna(n = 4),
    "acgt"
  )
  expect_equal(
    create_blocked_dna(n = 8),
    "aaccggtt"
  )
})

test_that("abuse", {
  expect_error(
    create_blocked_dna(n = "nonsense"),
    "'n' must be numerical"
  )
  expect_error(
    create_blocked_dna(n = -4),
    "'n' must be a positive non-zero number"
  )
  expect_error(
    create_blocked_dna(n = 1),
    "'n' must be a multitude of four"
  )
})
