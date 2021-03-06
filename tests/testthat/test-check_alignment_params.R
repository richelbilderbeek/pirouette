test_that("minimal use", {

  expect_silent(
    check_alignment_params(
      create_alignment_params()
    )
  )
})

test_that("all list elements must be present", {
  good_alignment_params <- create_alignment_params()

  alignment_params <- good_alignment_params
  alignment_params$root_sequence <- NULL
  expect_error(
    check_alignment_params(
      alignment_params
    ),
    "'root_sequence' must be an element of an 'alignment_params'"
  )

  alignment_params <- good_alignment_params
  alignment_params$rng_seed <- NULL
  expect_error(
    check_alignment_params(alignment_params),
    "'rng_seed' must be an element of an 'alignment_params'"
  )

  alignment_params <- good_alignment_params
  alignment_params$sim_tral_fun <- NULL
  expect_error(
    check_alignment_params(alignment_params),
    "'sim_tral_fun' must be an element of an 'alignment_params'"
  )

})

test_that("all list elements must have the right data type", {

  # 'root_sequence' is checked by 'check_root_sequence'
  # 'mutation_rate' is checked by 'check_mutation_rate'

  expect_error(
    check_alignment_params(
      create_alignment_params(
        root_sequence = "acgt",
        rng_seed = "nonsense"
      )
    ),
    "'rng_seed' must be a number"
  )
})

test_that("adding sim_tral_fun", {

  expect_silent(
    check_alignment_params(
      create_alignment_params(
        sim_tral_fun =
          sim_tral_with_std_nsm
      )
    )
  )
})
