context("test-create_experiment")

test_that("use", {

  model_type <- "generative"
  run_if <- "always"
  do_measure_evidence <- FALSE
  inference_model <- beautier::create_inference_model(
    tipdates_filename = "something_special.csv"
  )
  beast2_options <- create_beast2_options(rng_seed = 42)

  experiment <- create_experiment(
    model_type = model_type,
    run_if = run_if,
    do_measure_evidence = do_measure_evidence,
    inference_model = inference_model,
    beast2_options = beast2_options
  )

  expect_equal(model_type, experiment$model_type)
  expect_equal(run_if, experiment$run_if)
  expect_equal(do_measure_evidence, experiment$do_measure_evidence)
  expect_equal(inference_model, experiment$inference_model)
  expect_equal(beast2_options, experiment$beast2_options)
})
