context("test-check_model_select_params")

test_that("use, generative", {

  expect_silent(
    pirouette:::check_model_select_params(
      list(
        create_gen_model_select_param(
          alignment_params = create_alignment_params(
            root_sequence = "aaaa",
            mutation_rate = 0.1
          )
        )
      )
    )
  )
})

test_that("use, most evidence", {

  expect_silent(
    pirouette:::check_model_select_params(
      list(create_best_model_select_param())
    )
  )
})

test_that("abuse", {

  expect_error(
    pirouette:::check_model_select_params("nonsense"),
    "'model_select_params' must be a list"
  )

  expect_error(
    pirouette:::check_model_select_params(
      create_best_model_select_param()
    ),
    "'type' must be an element of a 'model_select_param'"
  )

})
