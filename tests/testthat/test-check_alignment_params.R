test_that("minimal use", {

  testthat::expect_silent(
    pirouette::check_alignment_params(
      pirouette::create_alignment_params()
    )
  )
})

test_that("all list elements must be present", {
  good_alignment_params <- pirouette::create_alignment_params()

  alignment_params <- good_alignment_params
  alignment_params$root_sequence <- NULL
  testthat::expect_error(
    pirouette::check_alignment_params(
      alignment_params
    ),
    "'root_sequence' must be an element of an 'alignment_params'"
  )

  alignment_params <- good_alignment_params
  alignment_params$rng_seed <- NULL
  testthat::expect_error(
    pirouette::check_alignment_params(alignment_params),
    "'rng_seed' must be an element of an 'alignment_params'"
  )

  alignment_params <- good_alignment_params
  alignment_params$sim_true_alignment_fun <- NULL
  testthat::expect_error(
    pirouette::check_alignment_params(alignment_params),
    "'sim_true_alignment_fun' must be an element of an 'alignment_params'"
  )

})

test_that("all list elements must have the right data type", {

  # 'root_sequence' is checked by 'check_root_sequence'
  # 'mutation_rate' is checked by 'check_mutation_rate'

  testthat::expect_error(
    pirouette::check_alignment_params(
      pirouette::create_alignment_params(
        root_sequence = "acgt",
        rng_seed = "nonsense"
      )
    ),
    "'rng_seed' must be a number"
  )
})

test_that("adding sim_true_alignment_fun", {

  testthat::expect_silent(
    pirouette::check_alignment_params(
      pirouette::create_alignment_params(
        sim_true_alignment_fun =
          sim_true_alignment_with_std_site_model
      )
    )
  )
})
