context("test-check_error_measure_params")

test_that("use", {

  good_error_measure_params <- create_error_measure_params()

  # OK
  expect_silent(
    check_error_measure_params(
      good_error_measure_params
    )
  )

  # Wrong parameter names
  error_measure_params <- good_error_measure_params
  error_measure_params$burn_in_fraction <- NULL
  expect_error(
    check_error_measure_params(
      error_measure_params
    ),
    "'burn_in_fraction' must be an element of an 'error_measure_params'"
  )

  error_measure_params <- good_error_measure_params
  error_measure_params$error_function <- NULL
  expect_error(
    check_error_measure_params(
      error_measure_params
    ),
    "'error_function' must be an element of an 'error_measure_params'"
  )

  # Wrong parameter values
  expect_error(
    check_error_measure_params(
      create_error_measure_params(
        burn_in_fraction = "nonsense"
      )
    ),
    "'burn_in_fraction' must be a number"
  )
  expect_error(
    check_error_measure_params(
      create_error_measure_params(
        burn_in_fraction = 1234.4567
      )
    ),
    "'burn_in_fraction' must be between 0.0 and 1.0"
  )
  # Checked in detail by 'check_error_function'
  expect_error(
    check_error_measure_params(
      create_error_measure_params(
        error_function = "nonsense"
      )
    ),
    "'error_function' must be a function"
  )
})
