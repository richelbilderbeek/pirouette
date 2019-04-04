context("test-check_alignment_params")

test_that("use", {

  good_alignment_params <- create_alignment_params(
    root_sequence = "acgt",
    mutation_rate = 0.1
  )

  # OK
  expect_silent(
    check_alignment_params(
      good_alignment_params
    )
  )

  # Wrong parameter names
  alignment_params <- good_alignment_params
  alignment_params$root_sequence <- NULL
  expect_error(
    check_alignment_params(
      alignment_params
    ),
    "'root_sequence' must be an element of an 'alignment_params'"
  )

  alignment_params <- good_alignment_params
  alignment_params$mutation_rate <- NULL
  expect_error(
    check_alignment_params(
      alignment_params
    ),
    "'mutation_rate' must be an element of an 'alignment_params'"
  )

  alignment_params <- good_alignment_params
  alignment_params$rng_seed <- NULL
  expect_error(
    check_alignment_params(alignment_params),
    "'rng_seed' must be an element of an 'alignment_params'"
  )

  alignment_params <- good_alignment_params
  alignment_params$site_model <- NULL
  expect_error(
    check_alignment_params(alignment_params),
    "'site_model' must be an element of an 'alignment_params'"
  )

  alignment_params <- good_alignment_params
  alignment_params$clock_model <- NULL
  expect_error(
    check_alignment_params(alignment_params),
    "'clock_model' must be an element of an 'alignment_params'"
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
  expect_error(
    check_alignment_params(
      create_alignment_params(
        clock_model = "nonsense"
      )
    ),
    "'clock_model' must be a clock model"
  )
  expect_error(
    check_alignment_params(
      create_alignment_params(
        clock_model = create_rln_clock_model()
      )
    ),
    "Unsupported 'clock_model'"
  )
})
