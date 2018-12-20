context("test-check_alignment_params")

test_that("use", {

  expect_silent(
    check_alignment_params(
      create_alignment_params(
        root_sequence = "acgt",
        mutation_rate = 0.1
      )
    )
  )

  expect_error(
    check_alignment_params(
      create_alignment_params(
        root_sequence = "nonsense",
        mutation_rate = 0.1
      )
    ),
    "'root_sequence' must be a lowercase DNA character string"
  )
  expect_error(
    check_alignment_params(
      create_alignment_params(
        root_sequence = "acgt",
        mutation_rate = -123.456
      )
    ),
    "'mutation_rate' must be a non-zero and positive value"
  )
})
