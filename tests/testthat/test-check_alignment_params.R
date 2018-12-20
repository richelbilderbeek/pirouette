context("test-check_alignment_params")

test_that("use", {

  alignment_params <- create_alignment_params(
    root_sequence = "acgt",
    mutation_rate = 0.1
  )

  # OK
  expect_silent(
    check_alignment_params(
      alignment_params
    )
  )

  # Wrong parameter names
  expect_error(
    check_alignment_params(
      alignment_params[-1]
    ),
    "'root_sequence' must be an element of an 'alignment_params'"
  )
  expect_error(
    check_alignment_params(
      alignment_params[-2]
    ),
    "'mutation_rate' must be an element of an 'alignment_params'"
  )
  expect_error(
    check_alignment_params(
      alignment_params[-3]
    ),
    "'rng_seed' must be an element of an 'alignment_params'"
  )

  # Wrong parameter values
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
  expect_error(
    check_alignment_params(
      create_alignment_params(
        root_sequence = "acgt",
        mutation_rate = 0.1,
        rng_seed = "nonsense"
      )
    ),
    "'rng_seed' must be a number"
  )
})
