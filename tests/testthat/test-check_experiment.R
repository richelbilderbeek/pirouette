context("test-check_experiment")

test_that("use", {

  expect_silent(check_experiment(create_experiment()))
})

test_that("wrong parameter names", {

  # Wrong parameter names
  experiment <- create_experiment()
  experiment$model_type <- NULL
  expect_error(
    check_experiment(
      experiment
    ),
    "'model_type' must be an element of an 'experiment'"
  )

  experiment <- create_experiment()
  experiment$run_if <- NULL
  expect_error(
    check_experiment(
      experiment
    ),
    "'run_if' must be an element of an 'experiment'"
  )

  experiment <- create_experiment()
  experiment$do_measure_evidence <- NULL
  expect_error(
    check_experiment(experiment),
    "'do_measure_evidence' must be an element of an 'experiment'"
  )

  experiment <- create_experiment()
  experiment$inference_model <- NULL
  expect_error(
    check_experiment(experiment),
    "'inference_model' must be an element of an 'experiment'"
  )

  experiment <- create_experiment()
  experiment$beast2_options <- NULL
  expect_error(
    check_experiment(experiment),
    "'beast2_options' must be an element of an 'experiment'"
  )
})

test_that("wrong parameter values", {

  expect_error(
    check_experiment(
      create_experiment(
        model_type = "nonsense"
      )
    ),
    "'model_type' must be either \"generative\" or \"candidate\""
  )
  expect_error(
    check_experiment(
      create_experiment(
        run_if = "nonsense"
      )
    ),
    "'run_if' must be either \"always\" or \"best_candidate\""
  )
  expect_error(
    check_experiment(
      create_experiment(
        do_measure_evidence = "nonsense"
      )
    ),
    "'do_measure_evidence' must be either TRUE or FALSE"
  )
  skip("WIP, #69")
  expect_error(
    check_experiment(
      create_experiment(
        inference_model = "nonsense"
      )
    ),
    "'inference_model' must be a valid inference model"
  )
  expect_error(
    check_experiment(
      create_experiment(
        beast2_options = "nonsense"
      )
    ),
    "'beast2_options' must be a valid inference model"
  )

})
