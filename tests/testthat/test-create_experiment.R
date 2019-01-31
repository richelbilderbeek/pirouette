context("test-create_experiment")

test_that("use", {
  skip("WIP #69")
  model_type <- "generative"
  run_if <- "always"
  do_measure_evidence <- FALSE

  experiment <- create_experiment(
    model_type = model_type,
    run_if = run_if,
    do_measure_evidence = do_measure_evidence,
    inference_model = beautier::create_inference_model()
  )

  expect_equal(model_type, experiment$model_type)
  expect_equal(run_if, experiment$run_if)
  expect_equal(do_measure_evidence, experiment$do_measure_evidence)
  expect_equal(inference_model, inference_model$model_type)
})
