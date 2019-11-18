context("test-select_experiments")

test_that("use, always", {
  experiments <- list(create_test_experiment())
  selected <- select_experiments(experiments)
  testthat::expect_equal(1, length(selected))
  check_experiments(selected)
})

test_that("use, most_evidence", {

  if (rappdirs::app_dir()$os == "win") return()

  marg_liks <- pirouette::create_test_marg_liks(
    site_models = list(beautier::create_jc69_site_model()),
    clock_models = list(beautier::create_strict_clock_model()),
    tree_priors =
      list(beautier::create_yule_tree_prior(), beautier::create_bd_tree_prior())
  )
  marg_liks$weight <- c(0.9, 0.1) # in favor of Yule

  experiment_bd <- pirouette::create_experiment(
    inference_conditions = pirouette::create_inference_conditions(
      model_type = "candidate",
      run_if = "best_candidate",
      do_measure_evidence = TRUE
    ),
    inference_model = beautier::create_inference_model(
      tree_prior = beautier::create_bd_tree_prior()
    )
  )
  experiment_yule <- pirouette::create_experiment(
    inference_conditions = pirouette::create_inference_conditions(
      model_type = "candidate",
      run_if = "best_candidate",
      do_measure_evidence = TRUE
    ),
    inference_model = beautier::create_inference_model(
      tree_prior = beautier::create_yule_tree_prior()
    )
  )
  experiments <- list(experiment_yule, experiment_bd)
  experiments[[1]]$beast2_options <- experiments[[2]]$beast2_options
  experiments[[1]]$errors_filename <- experiments[[2]]$errors_filename
  pirouette::check_experiments(experiments)

  selected <- pirouette::select_experiments(experiments, marg_liks)

  testthat::expect_equal(1, length(selected))
  testthat::expect_equal("yule", selected[[1]]$inference_model$tree_prior$name)
})

test_that("use, always", {
  # Select all experiments with 'run_if' is 'always'
  experiment <- create_experiment()
  experiment$inference_conditions$run_if <- "always"
  experiments <- list(experiment)
  selected <- select_experiments(experiments)
  testthat::expect_equal(1, length(selected))
})

test_that("generative model and candidate model", {

  # type       | run_if         | measure  | inference | evidence               # nolint this is no commented code
  #            |                | evidence | model     |
  # -----------|----------------|----------|-----------|----------
  # generative | always         |TRUE      |Yule       | 0.9                    # nolint this is no commented code
  # candidate  | best_candidate |TRUE      |Birth-Death| 0.1                    # nolint this is no commented code
  #
  # should select both models

  marg_liks <- pirouette::create_test_marg_liks(
    site_models = list(beautier::create_jc69_site_model()),
    clock_models = list(beautier::create_strict_clock_model()),
    tree_priors =
      list(beautier::create_yule_tree_prior(), beautier::create_bd_tree_prior())
  )
  marg_liks$weight <- c(0.9, 0.1) # in favor of generative model

  experiment_generative <- pirouette::create_experiment(
    inference_conditions = pirouette::create_inference_conditions(
      model_type = "generative",
      run_if = "always"
    ),
    inference_model = beautier::create_inference_model(
      tree_prior = beautier::create_yule_tree_prior()
    )
  )
  if (rappdirs::app_dir()$os != "win") {
    experiment_candidate <- pirouette::create_experiment(
      inference_conditions = pirouette::create_inference_conditions(
        model_type = "candidate",
        run_if = "best_candidate",
        do_measure_evidence = TRUE
      ),
      inference_model = beautier::create_inference_model(
        tree_prior = beautier::create_bd_tree_prior()
      )
    )
    experiments <- list(experiment_generative, experiment_candidate)
    selected <- pirouette::select_experiments(experiments, marg_liks)
    testthat::expect_equal(2, length(selected))
    testthat::expect_equal("yule", selected[[1]]$inference_model$tree_prior$name)
  } else {
    experiments <- list(experiment_generative)
    selected <- pirouette::select_experiments(experiments, marg_liks)
    testthat::expect_equal("yule", selected[[1]]$inference_model$tree_prior$name)
  }
})

test_that("use, verbose", {
  # Select all experiments with 'run_if' is 'always'
  experiment <- pirouette::create_experiment()
  experiment$inference_conditions$run_if <- "always"
  experiments <- list(experiment)
  output <- utils::capture.output(
    pirouette::select_experiments(experiments, verbose = TRUE)
  )
  testthat::expect_true(
    length(output) > 0 & !is.null(output)
  )
})
