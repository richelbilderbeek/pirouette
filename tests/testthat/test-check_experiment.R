context("test-check_experiment")

test_that("use", {

  expect_silent(check_experiment(create_experiment()))
})

test_that("wrong parameter names", {

  # Wrong parameter names
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
    "'beast2_options' must be valid BEAST2 options"
  )

  expect_error(
    check_experiment(
      create_experiment(
        inference_model = create_inference_model(
          mcmc = create_nested_sampling_mcmc()
        )
      )
    ),
    "An experiment's inference model must have a regular MCMC"
  )
  expect_error(
    check_experiment(
      create_experiment(
        est_evidence_mcmc = create_mcmc()
      )
    ),
    "'est_evidence_mcmc' must be a Nested Sampling MCMC"
  )
  # Wrong errors_filename
  expect_error(
    check_experiment(
      create_experiment(
        errors_filename = 12
      )
    ),
    "'errors_filename' must be a character vector"
  )
  expect_error(
    check_experiment(
      create_experiment(
        errors_filename = "pippo.fasta"
      )
    ),
    "'errors_filename' must be a csv file"
  )
})
