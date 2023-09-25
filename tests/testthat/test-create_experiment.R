context("test-create_experiment")

test_that("use", {

  model_type <- "generative"
  run_if <- "always"
  do_measure_evidence <- FALSE
  inference_model <- beautier::create_inference_model(
    tipdates_filename = "something_special.csv"
  )
  beast2_options <- beastier::create_beast2_options(rng_seed = 42)

  experiment <- create_experiment(
    inference_conditions = create_inference_conditions(
      model_type = model_type,
      run_if = run_if,
      do_measure_evidence = do_measure_evidence
    ),
    inference_model = inference_model,
    beast2_options = beast2_options
  )

  expect_equal(model_type, experiment$inference_conditions$model_type)
  expect_equal(run_if, experiment$inference_conditions$run_if)
  expect_equal(
    do_measure_evidence,
    experiment$inference_conditions$do_measure_evidence
  )
  expect_equal(inference_model, experiment$inference_model)
  expect_equal(beast2_options, experiment$beast2_options)
})

test_that("use", {

  model_type <- "generative"
  run_if <- "best_candidate"
  do_measure_evidence <- FALSE
  inference_model <- beautier::create_inference_model(
    tipdates_filename = "something_special.csv"
  )
  beast2_options <- beastier::create_beast2_options(rng_seed = 42)

  expect_error(
    experiment <- create_experiment(
      inference_conditions = create_inference_conditions(
        model_type = model_type,
        run_if = run_if,
        do_measure_evidence = do_measure_evidence
      ),
      inference_model = inference_model,
      beast2_options = beast2_options
    ),
    "'run_if' == 'best_candidate' and 'do_measure_evidence' == FALSE is a configuration that makes no sense" # nolint
  )
})
