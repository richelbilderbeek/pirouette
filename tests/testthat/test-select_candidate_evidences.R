context("test-select_candidate_evidences")

test_that("nothing if no candidates", {
  candidate_evidences <- select_candidate_evidences()
  expect_equal(0, nrow(candidate_evidences))
})

test_that("one if there is one candidate", {

  if (rappdirs::app_dir()$os == "win") return()

  experiment <- create_experiment(
    inference_conditions = create_inference_conditions(
      run_if = "best_candidate",
      model_type = "candidate",
      do_measure_evidence = TRUE
    )
  )
  expect_equal(experiment$inference_conditions$model_type, "candidate")

  candidate_evidences <- select_candidate_evidences(
    experiments = list(experiment),
    marg_liks = create_test_marg_liks()
  )
  expect_equal(1, nrow(candidate_evidences))
})

test_that("one if there is one candidate and one generative", {

  if (rappdirs::app_dir()$os == "win") return()

  experiment_1 <- create_test_gen_experiment()
  experiment_2 <- create_test_cand_experiment()
  # Candidates must have different model than generative model
  experiment_2$inference_model$site_model <-
    beautier::create_tn93_site_model()
  expect_equal(experiment_1$inference_conditions$model_type, "generative")
  expect_equal(experiment_2$inference_conditions$model_type, "candidate")
  experiments <- list(experiment_1, experiment_2)

  candidate_evidences <- select_candidate_evidences(
    experiments = experiments,
    marg_liks = create_test_marg_liks()
  )
  expect_equal(1, nrow(candidate_evidences))
})
