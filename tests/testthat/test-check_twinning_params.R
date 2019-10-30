context("test-check_twinning_params")

test_that("minimal use", {

  expect_silent(check_twinning_params(create_twinning_params()))
})

test_that("element names", {

  good_twinning_params <- create_twinning_params()

  # OK
  expect_silent(
    check_twinning_params(
      good_twinning_params
    )
  )

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
  twinning_params$sim_twin_tree_function <- NULL
  expect_error(
    check_twinning_params(
      twinning_params
    ),
    "'sim_twin_tree_function' must be an element of an 'twinning_params'"
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

})

test_that("element data types", {

  good_twinning_params <- create_twinning_params()

  # OK
  expect_silent(
    check_twinning_params(
      good_twinning_params
    )
  )

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


})

test_that("element values", {

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


test_that("add sim_twin_tree_function", {

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

test_that("add sim_twin_alignment_function", {

  expect_silent(
    check_twinning_params(
      create_twinning_params(
        sim_twin_alignment_function = get_default_sim_twin_alignment_function()
      )
    )
  )
})
