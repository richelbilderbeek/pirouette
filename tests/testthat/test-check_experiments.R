test_that("use", {

  expect_silent(check_experiments(list(create_experiment())))
})

test_that("each element in the list must be a proper experiment", {

  experiment_1 <- create_experiment()
  experiment_2 <- "nonsense"
  experiments <- list(experiment_1, experiment_2)
  expect_error(
    check_experiments(experiments)
  )
})

test_that("must have same MCMC chain length", {

  experiment_1 <- create_experiment()
  experiment_2 <- create_experiment()
  experiment_2$inference_model$mcmc$chain_length <-
    experiment_2$inference_model$mcmc$chain_length * 10
  experiments <- list(experiment_1, experiment_2)
  expect_error(
    check_experiments(experiments),
    "All MCMCs in the experiments must be identical"
  )
})

test_that("correct order of experiments", {

  if (rappdirs::app_dir()$os == "win") return()

  gen_exp <- create_test_gen_experiment()
  cand_exp <- create_test_cand_experiment()
  # Must use different inference model than generative model
  cand_exp$inference_model$site_model <- beautier::create_gtr_site_model()
  # Must have same MCMC
  cand_exp$inference_model$mcmc <- gen_exp$inference_model$mcmc

  # OK order
  expect_silent(
    check_experiments(
      experiments = list(gen_exp, cand_exp)
    )
  )

  # Generative must be first
  expect_error(
    check_experiments(
      experiments = list(cand_exp, gen_exp)
    ),
    "If multiple experiments, generative is either first or absent"
  )

  # Only one generative experiment
  expect_error(
    check_experiments(
      experiments = list(gen_exp, gen_exp)
    ),
    "Specifying more than one 'generative' model experiment is redundant"
  )
})

test_that("same beast2_options_filenames and error fileanames in candidates", {

  if (rappdirs::app_dir()$os == "win") return()

  cand_experiment_1 <- create_test_cand_experiment()
  cand_experiment_2 <- create_test_cand_experiment()
  gen_experiment <- create_test_gen_experiment()
  # Must use different inference model than generative model
  cand_experiment_1$inference_model$site_model <-
    beautier::create_tn93_site_model()
  cand_experiment_2$inference_model$site_model <-
    beautier::create_gtr_site_model()
  # Must have same MCMC
  cand_experiment_1$inference_model$mcmc <- cand_experiment_2$inference_model$mcmc
  gen_experiment$inference_model$mcmc <- cand_experiment_2$inference_model$mcmc

  expect_silent(
    check_experiments(list(gen_experiment, cand_experiment_1))
  )
  expect_silent(
    check_experiments(list(gen_experiment, cand_experiment_2))
  )
  expect_error(
    check_experiments(list(cand_experiment_1, cand_experiment_2)),
    "Candidate models must have same BEAST2 input filename"
  )
  expect_error(
    check_experiments(
      list(gen_experiment, cand_experiment_1, cand_experiment_2)
    ),
    "Candidate models must have same BEAST2 input filename"
  )

  # Fix BEAST2 options, as those hold the file location
  cand_experiment_1$beast2_options <- cand_experiment_2$beast2_options
  gen_experiment$beast2_options <- cand_experiment_2$beast2_options

  expect_error(
    check_experiments(experiments = list(cand_experiment_1, cand_experiment_2)),
    "Candidate models must have same errors filename"
  )
  # Fix error filenames
  cand_experiment_1$errors_filename <- cand_experiment_2$errors_filename

  expect_silent(
    check_experiments(list(cand_experiment_1, cand_experiment_2))
  )
  expect_silent(
    check_experiments(
      list(gen_experiment, cand_experiment_1, cand_experiment_2)
    )
  )
})

test_that("differ each beast2_options_filename and error filename", {

  if (rappdirs::app_dir()$os == "win") return()

  experiment_1 <- create_test_cand_experiment()
  good_experiment_2 <- experiment_1
  # Experiments must have different inference models
  good_experiment_2$inference_model$site_model <-
    beautier::create_gtr_site_model()

  experiment_2 <- good_experiment_2
  expect_silent(check_experiments(list(experiment_1, experiment_2)))

  # BEAST2 input filename
  experiment_2$beast2_options$input_filename <- "different"
  expect_error(
    check_experiments(list(experiment_1, experiment_2)),
    "Candidate models must have same BEAST2 input filename"
  )
  experiment_2 <- good_experiment_2
  expect_silent(check_experiments(list(experiment_1, experiment_2)))

  # BEAST2 output log filename
  experiment_2$inference_model$mcmc$tracelog$filename <- "different"
  expect_error(
    check_experiments(list(experiment_1, experiment_2)),
    "Candidate models must have same BEAST2 output log filename"
  )
  experiment_2 <- good_experiment_2
  expect_silent(check_experiments(list(experiment_1, experiment_2)))

  # BEAST2 output trees filenames
  experiment_2$inference_model$mcmc$treelog$filename <- "different"
  expect_error(
    check_experiments(list(experiment_1, experiment_2)),
    "Candidate models must have same BEAST2 output trees filename"
  )
  experiment_2 <- good_experiment_2
  expect_silent(check_experiments(list(experiment_1, experiment_2)))

  # BEAST2 output state
  experiment_2$beast2_options$output_state_filename <- "different"
  expect_error(
    check_experiments(list(experiment_1, experiment_2)),
    "Candidate models must have same BEAST2 output state filename"
  )
  experiment_2 <- good_experiment_2
  expect_silent(check_experiments(list(experiment_1, experiment_2)))

  # Errors filename
  experiment_2$errors_filename <- "different.csv"
  expect_error(
    check_experiments(list(experiment_1, experiment_2)),
    "Candidate models must have same errors filename"
  )
  experiment_2 <- good_experiment_2
  expect_silent(check_experiments(list(experiment_1, experiment_2)))
})

test_that("detect same model in generative and candidate model", {

  if (rappdirs::app_dir()$os == "win") return()

  inference_model <- create_inference_model()
  gen_exp <- create_test_gen_experiment(
    inference_model = inference_model
  )
  check_experiment(gen_exp)

  cand_exp <- create_test_cand_experiment(
    inference_model = inference_model
  )
  check_experiment(cand_exp)

  experiments <- list(gen_exp, cand_exp)

  expect_error(
    check_experiments(experiments),
    "All inference models must be unique"
  )
})

test_that("detect same model in two candidate models", {

  if (rappdirs::app_dir()$os == "win") return()

  cand_exp_1 <- create_test_cand_experiment()
  cand_exp_2 <- cand_exp_1
  experiments <- list(cand_exp_1, cand_exp_2)
  expect_error(
    check_experiments(experiments),
    "All inference models must be unique"
  )
})
