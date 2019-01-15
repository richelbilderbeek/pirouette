context("test-check_twinning_params")

test_that("use", {

  good_twinning_params <- create_twinning_params()

  # OK
  expect_silent(
    check_twinning_params(
      good_twinning_params
    )
  )

  # Wrong parameter names
  twinning_params <- good_twinning_params
  twinning_params$rng_seed <- NULL
  expect_error(
    check_twinning_params(
      twinning_params
    ),
    "'rng_seed' must be an element of an 'twinning_params'"
  )

  twinning_params <- good_twinning_params
  twinning_params$twin_tree_filename <- NULL
  expect_error(
    check_twinning_params(
      twinning_params
    ),
    "'twin_tree_filename' must be an element of an 'twinning_params'"
  )

  # Wrong parameter values
  expect_error(
    check_twinning_params(
      create_twinning_params(
        rng_seed = "nonsense"
      )
    ),
    "'rng_seed' must be a number"
  )
  expect_error(
    check_twinning_params(
      create_twinning_params(
        twin_tree_filename = NA
      )
    ),
    "'twin_tree_filename' must be a character vector"
  )
})
