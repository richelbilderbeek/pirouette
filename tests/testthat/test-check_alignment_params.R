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

  alignment_params <- good_alignment_params
  alignment_params$clock_model <- NULL
  expect_error(
    check_alignment_params(alignment_params),
    "'clock_model' must be an element of an 'alignment_params'"
  )
})

test_that("all list elements must have the right data type", {
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
        mutation_rate = function(phylogeny) "nonsense"
      )
    ),
    "'mutation_rate' function must return a number"
  )

  expect_error(
    check_alignment_params(
      create_alignment_params(
        root_sequence = "acgt",
        mutation_rate = function(phylogeny) -1234567
      )
    ),
    "'mutation_rate' function must return non-zero and positive value"
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
