test_that("minimal use", {
  pir_params <- create_test_pir_params()
  testit::assert(length(pir_params$experiments) == 1)
  testit::assert(
    pir_params$experiments[[1]]$inference_conditions$model_type ==
      "generative"
  )
  expect_false(has_candidate_experiments(pir_params))
})

test_that("gen", {

  pir_params <- create_test_pir_params_setup(
    has_candidate = FALSE
  )
  expect_true(!has_candidate_experiments(pir_params))
})

test_that("gen + cand", {

  if (rappdirs::app_dir()$os == "win") return()

  pir_params <- create_test_pir_params_setup(
    has_candidate = TRUE
  )
  expect_true(has_candidate_experiments(pir_params))
})
