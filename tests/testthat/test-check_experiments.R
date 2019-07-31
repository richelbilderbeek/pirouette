context("test-check_experiments")

test_that("use", {

  expect_silent(check_experiments(experiments = list(create_experiment())))
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
  check_experiment(gen_exp)

  cand_exp <- create_test_cand_experiment()
  check_experiment(cand_exp)

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
  expect_silent(
    check_experiments(list(gen_experiment, cand_experiment_1))
  )
  expect_silent(
    check_experiments(list(gen_experiment, cand_experiment_2))
  )
  expect_silent(
    check_experiments(list(cand_experiment_1, cand_experiment_1))
  )
  expect_silent(
    check_experiments(list(cand_experiment_2, cand_experiment_2))
  )
  expect_error(
    check_experiments(list(cand_experiment_1, cand_experiment_2)),
    "Candidate models must have same beast2_options filename"
  )
  expect_error(
    check_experiments(
      list(gen_experiment, cand_experiment_1, cand_experiment_2)
    ),
    "Candidate models must have same beast2_options filename"
  )

  # Fix BEAST2 options
  cand_experiment_1$beast2_options <- cand_experiment_2$beast2_options

  expect_error(
    check_experiments(list(cand_experiment_1, cand_experiment_2)),
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
