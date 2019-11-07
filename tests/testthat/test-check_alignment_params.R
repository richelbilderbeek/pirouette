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

})

test_that("all list elements must have the right data type", {

  # 'root_sequence' is checked by 'check_root_sequence'
  # 'mutation_rate' is checked by 'check_mutation_rate'

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

  # site_model
  expect_silent(
    check_alignment_params(
      create_alignment_params(
        site_model = beautier::create_jc69_site_model()
      )
    )
  )
  expect_silent(
    check_alignment_params(
      create_alignment_params(
        site_model = beautier::create_hky_site_model()
      )
    )
  )
  expect_silent(
    check_alignment_params(
      create_alignment_params(
        site_model = beautier::create_tn93_site_model()
      )
    )
  )
  expect_silent(
    check_alignment_params(
      create_alignment_params(
        site_model = beautier::create_gtr_site_model()
      )
    )
  )
  expect_silent(
    check_alignment_params(
      create_alignment_params(
        site_model = "linked_node_sub"
      )
    )
  )
  expect_silent(
    check_alignment_params(
      create_alignment_params(
        site_model = "unlinked_node_sub"
      )
    )
  )

  expect_error(
    check_alignment_params(
      create_alignment_params(
        site_model = "nonsense"
      )
    ),
    "'site_model' must be a site model"
  )
})

test_that("adding sim_true_alignment_function", {

  expect_silent(
    check_alignment_params(
      create_alignment_params(
        sim_true_alignment_function =
          sim_true_alignment_with_standard_site_model
      )
    )
  )
})
