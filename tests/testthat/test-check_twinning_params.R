context("test-check_twinning_params")

test_that("use", {

  good_twinning_params <- create_twinning_params()

  # OK
  expect_silent(
    check_twinning_params(
      good_twinning_params
    )
  )

  ##############################################################################
  # Missing elements
  ##############################################################################
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
  twinning_params$twin_model <- NULL
  expect_error(
    check_twinning_params(
      twinning_params
    ),
    "'twin_model' must be an element of an 'twinning_params'"
  )

  twinning_params <- good_twinning_params
  twinning_params$method <- NULL
  expect_error(
    check_twinning_params(
      twinning_params
    ),
    "'method' must be an element of an 'twinning_params'"
  )

  twinning_params <- good_twinning_params
  twinning_params$n_replicates <- NULL
  expect_error(
    check_twinning_params(
      twinning_params
    ),
    "'n_replicates' must be an element of an 'twinning_params'"
  )

  twinning_params <- good_twinning_params
  twinning_params$twin_tree_filename <- NULL
  expect_error(
    check_twinning_params(
      twinning_params
    ),
    "'twin_tree_filename' must be an element of an 'twinning_params'"
  )

  twinning_params <- good_twinning_params
  twinning_params$twin_alignment_filename <- NULL
  expect_error(
    check_twinning_params(
      twinning_params
    ),
    "'twin_alignment_filename' must be an element of an 'twinning_params'"
  )

  twinning_params <- good_twinning_params
  twinning_params$twin_evidence_filename <- NULL
  expect_error(
    check_twinning_params(
      twinning_params
    ),
    "'twin_evidence_filename' must be an element of an 'twinning_params'"
  )

  # Wrong parameter values
  expect_error(
    check_twinning_params(
      create_twinning_params(
        rng_seed_twin_tree = "nonsense"
      )
    ),
    "'rng_seed_twin_tree' must be a whole number"
  )
  expect_error(
    check_twinning_params(
      create_twinning_params(
        rng_seed_twin_alignment = "nonsense"
      )
    ),
    "'rng_seed_twin_alignment' must be a whole number"
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

  ##############################################################################
  # Wrong element data types
  ##############################################################################
  # rng_seed_twin_tree
  expect_error(
    check_twinning_params(
      create_twinning_params(
        rng_seed_twin_tree = "nonsense"
      )
    ),
    "'rng_seed_twin_tree' must be a whole number"
  )
  expect_error(
    check_twinning_params(
      create_twinning_params(
        rng_seed_twin_tree = 3.14
      )
    ),
    "'rng_seed_twin_tree' must be a whole number"
  )

  # rng_seed_twin_alignment
  expect_error(
    check_twinning_params(
      create_twinning_params(
        rng_seed_twin_alignment = "nonsense"
      )
    ),
    "'rng_seed_twin_alignment' must be a whole number"
  )
  expect_error(
    check_twinning_params(
      create_twinning_params(
        rng_seed_twin_alignment = 3.14
      )
    ),
    "'rng_seed_twin_alignment' must be a whole number"
  )

  # twin_model
  expect_silent(
    check_twinning_params(
      create_twinning_params(twin_model = "yule")
    )
  )
  expect_silent(
    check_twinning_params(
      create_twinning_params(twin_model = "birth_death")
    )
  )
  expect_silent(
    check_twinning_params(
      create_twinning_params(twin_model = "copy_true")
    )
  )
  expect_error(
    check_twinning_params(
      create_twinning_params(
        twin_model = "nonsense"
      )
    ),
    "'twin_model' is not implemented"
  )

  # method
  expect_error(
    check_twinning_params(
      create_twinning_params(
        method = "nonsense"
      )
    ),
    "'method' is not implemented"
  )

  # n_replicates
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

  # twin_tree_filename
  expect_error(
    check_twinning_params(
      create_twinning_params(
        twin_tree_filename = 13
      )
    ),
    "'twin_tree_filename' must be a character vector"
  )

  # twin_alignment_filename
  expect_error(
    check_twinning_params(
      create_twinning_params(
        twin_alignment_filename = 13
      )
    ),
    "'twin_alignment_filename' must be a character vector"
  )

  # twin_evidence_filename
  expect_error(
    check_twinning_params(
      create_twinning_params(
        twin_evidence_filename = 13
      )
    ),
    "'twin_evidence_filename' must be a character vector"
  )
})


test_that("use", {

  expect_silent(create_sim_yule_twin_tree_function())
  expect_silent(create_sim_bd_twin_tree_function())
  expect_silent(create_copy_twin_tree_from_true_function())
  skip("#340")

  expect_silent(
    create_twinning_params(
      sim_twin_tree_function = create_sim_yule_twin_tree_function()
    )
  )
  expect_silent(
    create_twinning_params(
      sim_twin_tree_function = create_sim_bd_twin_tree_function()
    )
  )
  expect_silent(
    create_twinning_params(
      sim_twin_tree_function = create_copy_twin_tree_from_true_function()
    )
  )

})

