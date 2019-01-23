context("test-create_error_measure_params")

test_that("use", {
  expect_silent(create_error_measure_params())
})

test_that("abuse", {

  # Exact error messages checked by 'check_error_measure_params]
  expect_error(
    create_error_measure_params(
      rng_seed = "nonsense"
    )
  )
  expect_error(
    create_error_measure_params(
      twin_tree_filename = NA
    )
  )
})
