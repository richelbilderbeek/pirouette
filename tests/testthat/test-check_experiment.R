context("test-check_experiment")

test_that("use", {
  expect_silent(check_experiment(create_experiment()))
  expect_silent(check_experiment(create_gen_experiment()))

  if (rappdirs::app_dir()$os == "win") return()
  expect_silent(check_experiment(create_cand_experiment()))
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
        inference_conditions = "nonsense"
      )
    ),
    "'inference_conditions' must be a valid inference_conditions"
  )

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
        inference_model = beautier::create_inference_model(
          mcmc = beautier::create_ns_mcmc()
        )
      )
    ),
    "An experiment's inference model must have a regular MCMC"
  )
  expect_error(
    check_experiment(
      create_experiment(
        est_evidence_mcmc = beautier::create_mcmc()
      )
    ),
    "'est_evidence_mcmc' must be a Nested Sampling MCMC"
  )
  expect_error(
    check_experiment(
      create_experiment(
        beast2_bin_path = "nonsense"
      )
    ),
    "'beast2_bin_path' must be a path to a BEAST2 binary file."
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
