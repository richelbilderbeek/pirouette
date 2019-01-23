context("test-create_blocked_dna")

test_that("use", {
  expect_equal(
    create_blocked_dna(length = 4),
    "acgt"
  )
  expect_equal(
    create_blocked_dna(length = 8),
    "aaccggtt"
  )
})

test_that("abuse", {
  expect_error(
    create_blocked_dna(length = "nonsense"),
    "'length' must be numerical"
  )
  expect_error(
    create_blocked_dna(length = -4),
    "'length' must be a positive non-zero number"
  )
  expect_error(
    create_blocked_dna(length = 1),
    "'length' must be a multitude of four"
  )
})
