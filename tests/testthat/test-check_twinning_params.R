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
  twinning_params$rng_seed_twin_tree <- NULL
  expect_error(
    check_twinning_params(
      twinning_params
    ),
    "'rng_seed_twin_tree' must be an element of an 'twinning_params'"
  )

  twinning_params <- good_twinning_params
  twinning_params$rng_seed_twin_alignment <- NULL
  expect_error(
    check_twinning_params(
      twinning_params
    ),
    "'rng_seed_twin_alignment' must be an element of an 'twinning_params'"
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
        rng_seed_twin_tree = "nonsense"
      )
    ),
    "'rng_seed_twin_tree' must be a number"
  )
  expect_error(
    check_twinning_params(
      create_twinning_params(
        rng_seed_twin_alignment = "nonsense"
      )
    ),
    "'rng_seed_twin_alignment' must be a number"
  )
  expect_error(
    check_twinning_params(
      create_twinning_params(
        twin_tree_filename = NA
      )
    ),
    "'twin_tree_filename' must be a character vector"
  )

  # Wrong methods
  expect_error(
    check_twinning_params(
      create_twinning_params(
        method = 12
      )
    ),
    "'method' must be a character vector"
  )
  expect_error(
    check_twinning_params(
      create_twinning_params(
        method = "nonsense"
      )
    ),
    "This 'method' is not implemented"
  )

  # Wrong methods
  expect_error(
    check_twinning_params(
      create_twinning_params(
        n_replicates = "nonsense"
      )
    ),
    "'n_replicates' must be a whole number"
  )
  expect_error(
    check_twinning_params(
      create_twinning_params(
        n_replicates = 1.5
      )
    ),
    "'n_replicates' must be a whole number"
  )
  expect_error(
    check_twinning_params(
      create_twinning_params(
        n_replicates = Inf
      )
    ),
    "'n_replicates' must be a whole number"
  )
  expect_error(
    check_twinning_params(
      create_twinning_params(
        n_replicates = -10
      )
    ),
    "'n_replicates' must be a finite positive integer number"
  )

  # Wrong twin_evidence_filename
  expect_error(
    check_twinning_params(
      create_twinning_params(
        twin_evidence_filename = 13
      )
    ),
    "'twin_evidence_filename' must be a character vector"
  )
})
