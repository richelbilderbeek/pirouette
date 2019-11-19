test_that("minimal use", {
  pir_params <- pirouette::create_test_pir_params()
  testit::assert(length(pir_params$experiments) == 1)
  testit::assert(
    pir_params$experiments[[1]]$inference_conditions$model_type ==
      "generative"
  )
  testthat::expect_false(pirouette::has_candidate_experiments(pir_params))
})

test_that("one candidate model", {

  if (rappdirs::app_dir()$os == "win") return()

  pir_params <- pirouette::create_test_pir_params(
    experiments = list(pirouette::create_test_cand_experiment())
  )
  testthat::expect_true(pirouette::has_candidate_experiments(pir_params))
})

test_that("gen + cand", {

  if (rappdirs::app_dir()$os == "win") return()

  pir_params <- create_test_pir_params(
    experiments = list(
      pirouette::create_test_gen_experiment(),
      pirouette::create_test_cand_experiment(
        inference_model = beautier::create_test_inference_model(
          site_model = beautier::create_gtr_site_model()
        )
      )
    )
  )
  testthat::expect_true(has_candidate_experiments(pir_params))
})
