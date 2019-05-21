context("test-create_twinning_params")

test_that("use", {
  expect_silent(create_twinning_params())
})

test_that("Twin tree should be BD by default, #161", {
  twinning_params <- create_twinning_params()
  expect_equal(twinning_params$twin_model, "birth_death")
})

test_that("abuse", {

  # Exact error messages checked by 'check_twinning_params]
  expect_error(
    create_twinning_params(
      rng_seed = "nonsense"
    )
  )
  expect_error(
    create_twinning_params(
      twin_model = 42
    )
  )
  expect_error(
    create_twinning_params(
      twin_model = "nonsense"
    )
  )
  expect_error(
    create_twinning_params(
      twin_tree_filename = NA
    )
  )
  expect_error(
    create_twinning_params(
      twin_alignment_filename = 42
    )
  )
})
