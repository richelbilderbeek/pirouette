test_that("minimal use", {
  pir_params <- create_test_pir_params()
  testit::assert(length(pir_params$experiments) == 1)
  testit::assert(
    pir_params$experiments[[1]]$inference_conditions$model_type ==
      "generative"
  )
  expect_false(has_candidate_experiments(pir_params))
})

test_that("one candidate model", {

  if (rappdirs::app_dir()$os == "win") return()

  pir_params <- create_test_pir_params(
    experiments = list(create_test_cand_experiment())
  )
  expect_true(has_candidate_experiments(pir_params))
})

test_that("gen + cand", {
  pir_params <- create_test_pir_params(
    experiments = list(
      create_test_gen_experiment(),
      create_test_cand_experiment(
        inference_model = create_test_inference_model(
          site_model = beautier::create_gtr_site_model()
        )
      )
    )
  )
  expect_true(has_candidate_experiments(pir_params))
})
