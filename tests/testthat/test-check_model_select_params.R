context("test-check_model_select_params")

test_that("use, generative", {

  expect_silent(
    pirouette:::check_model_select_params(
      create_gen_model_select_params(
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
    pirouette:::check_model_select_params(
      create_most_evidence_model_select_params()
    )
  )
})

test_that("abuse", {

  expect_error(
    pirouette:::check_model_select_params(
      create_model_select_params(model_selections = "nonsense")
    ),
    "All elements of 'model_select_params\\$model_selections' must be in 'get_model_selections\\(\\)'" # nolint long indeed
  )

  expect_error(
    pirouette:::check_model_select_params(
      create_model_select_params(site_models = "nonsense")
    )
  )

  expect_error(
    pirouette:::check_model_select_params(
      create_model_select_params(clock_models = "nonsense")
    )
  )

  expect_error(
    pirouette:::check_model_select_params(
      create_model_select_params(tree_priors = "nonsense")
    )
  )
})
