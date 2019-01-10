context("test-check_model_select_param")

test_that("use, generative", {

  expect_silent(
    pirouette:::check_model_select_param(
      create_gen_model_select_param(
        alignment_params = create_alignment_params(
          root_sequence = "aaaa",
          mutation_rate = 0.1
        )
      )
    )
  )
})

test_that("use, most evidence", {

  expect_silent(
    pirouette:::check_model_select_param(
      create_best_model_select_param()
    )
  )
})

test_that("abuse", {

  expect_error(
    pirouette:::check_model_select_param(
      create_model_select_param(type = "nonsense")
    ),
    "All elements of 'model_select_param\\$type' must be in 'get_model_selections\\(\\)'" # nolint long indeed
  )

  expect_error(
    pirouette:::check_model_select_param(
      create_model_select_param(site_models = "nonsense")
    )
  )

  expect_error(
    pirouette:::check_model_select_param(
      create_model_select_param(clock_models = "nonsense")
    )
  )

  expect_error(
    pirouette:::check_model_select_param(
      create_model_select_param(tree_priors = "nonsense")
    )
  )
})
