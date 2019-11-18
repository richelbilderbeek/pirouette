context("test-check_twinning_params")

test_that("minimal use", {

  testthat::expect_silent(
    pirouette::check_twinning_params(pirouette::create_twinning_params())
  )
})

test_that("element names", {

  good_twinning_params <- pirouette::create_twinning_params()

  # OK
  testthat::expect_silent(
    pirouette::check_twinning_params(
      good_twinning_params
    )
  )

  twinning_params <- good_twinning_params
  twinning_params$rng_seed_twin_tree <- NULL
  testthat::expect_error(
    pirouette::check_twinning_params(
      twinning_params
    ),
    "'rng_seed_twin_tree' must be an element of an 'twinning_params'"
  )

  twinning_params <- good_twinning_params
  twinning_params$rng_seed_twin_alignment <- NULL
  testthat::expect_error(
    pirouette::check_twinning_params(
      twinning_params
    ),
    "'rng_seed_twin_alignment' must be an element of an 'twinning_params'"
  )

  twinning_params <- good_twinning_params
  twinning_params$sim_twin_tree_fun <- NULL
  testthat::expect_error(
    pirouette::check_twinning_params(
      twinning_params
    ),
    "'sim_twin_tree_fun' must be an element of an 'twinning_params'"
  )

  twinning_params <- good_twinning_params
  twinning_params$twin_tree_filename <- NULL
  testthat::expect_error(
    pirouette::check_twinning_params(
      twinning_params
    ),
    "'twin_tree_filename' must be an element of an 'twinning_params'"
  )

  twinning_params <- good_twinning_params
  twinning_params$twin_alignment_filename <- NULL
  testthat::expect_error(
    pirouette::check_twinning_params(
      twinning_params
    ),
    "'twin_alignment_filename' must be an element of an 'twinning_params'"
  )

  twinning_params <- good_twinning_params
  twinning_params$twin_evidence_filename <- NULL
  testthat::expect_error(
    pirouette::check_twinning_params(
      twinning_params
    ),
    "'twin_evidence_filename' must be an element of an 'twinning_params'"
  )

})

test_that("element data types", {

  good_twinning_params <- pirouette::create_twinning_params()

  # OK
  testthat::expect_silent(
    pirouette::check_twinning_params(
      good_twinning_params
    )
  )

  testthat::expect_error(
    pirouette::check_twinning_params(
      pirouette::create_twinning_params(
        rng_seed_twin_tree = "nonsense"
      )
    ),
    "'rng_seed_twin_tree' must be a whole number"
  )
  testthat::expect_error(
    pirouette::check_twinning_params(
      pirouette::create_twinning_params(
        rng_seed_twin_alignment = "nonsense"
      )
    ),
    "'rng_seed_twin_alignment' must be a whole number"
  )
  testthat::expect_error(
    pirouette::check_twinning_params(
      pirouette::create_twinning_params(
        twin_tree_filename = NA
      )
    ),
    "'twin_tree_filename' must be a character vector"
  )

})

test_that("element values", {

  # rng_seed_twin_tree
  testthat::expect_error(
    pirouette::check_twinning_params(
      pirouette::create_twinning_params(
        rng_seed_twin_tree = "nonsense"
      )
    ),
    "'rng_seed_twin_tree' must be a whole number"
  )
  testthat::expect_error(
    pirouette::check_twinning_params(
      pirouette::create_twinning_params(
        rng_seed_twin_tree = 3.14
      )
    ),
    "'rng_seed_twin_tree' must be a whole number"
  )

  # rng_seed_twin_alignment
  testthat::expect_error(
    pirouette::check_twinning_params(
      pirouette::create_twinning_params(
        rng_seed_twin_alignment = "nonsense"
      )
    ),
    "'rng_seed_twin_alignment' must be a whole number"
  )
  testthat::expect_error(
    pirouette::check_twinning_params(
      pirouette::create_twinning_params(
        rng_seed_twin_alignment = 3.14
      )
    ),
    "'rng_seed_twin_alignment' must be a whole number"
  )

  # twin_tree_filename
  testthat::expect_error(
    pirouette::check_twinning_params(
      pirouette::create_twinning_params(
        twin_tree_filename = 13
      )
    ),
    "'twin_tree_filename' must be a character vector"
  )

  # twin_alignment_filename
  testthat::expect_error(
    pirouette::check_twinning_params(
      pirouette::create_twinning_params(
        twin_alignment_filename = 13
      )
    ),
    "'twin_alignment_filename' must be a character vector"
  )

  # twin_evidence_filename
  testthat::expect_error(
    pirouette::check_twinning_params(
      pirouette::create_twinning_params(
        twin_evidence_filename = 13
      )
    ),
    "'twin_evidence_filename' must be a character vector"
  )
})


test_that("add sim_twin_tree_fun", {

  testthat::expect_silent(
    pirouette::create_twinning_params(
      sim_twin_tree_fun = pirouette::create_sim_yule_twin_tree_fun()
    )
  )
  testthat::expect_silent(
    pirouette::create_twinning_params(
      sim_twin_tree_fun = pirouette::create_sim_bd_twin_tree_fun()
    )
  )
  testthat::expect_silent(
    pirouette::create_twinning_params(
      sim_twin_tree_fun = pirouette::create_copy_twin_tree_from_true_fun()
    )
  )

})

test_that("add sim_twin_alignment_fun", {

  testthat::expect_silent(
    pirouette::check_twinning_params(
      pirouette::create_twinning_params(
        sim_twin_alignment_fun =
          pirouette::get_sim_twin_alignment_with_std_site_model_fun()
      )
    )
  )
})
