context("test-create_all_experiments")

test_that("use", {
  if (rappdirs::app_dir()$os == "win")  return()

  # This test takes too long
  if (!beastier::is_on_ci()) return()

  all_experiments <- create_all_experiments()

  # no NULLs in this list please
  expect_true(
    all(vapply(all_experiments, FUN = is.null, FUN.VALUE = TRUE) == FALSE)
  )

  for (i in seq_along(all_experiments)) {
    experiment <- all_experiments[[i]]
    expect_silent(check_experiment(experiment))
  }
})

test_that("it can exclude an experiment", {
  if (rappdirs::app_dir()$os == "win")  return()

  # This test takes too long
  if (!beastier::is_on_ci()) return()

  all_experiments <- create_all_experiments(
    exclude_model = create_inference_model(
      site_model = create_site_model_jc69(),
      clock_model = create_strict_clock_model(),
      tree_prior = create_tree_prior_bd(),
      mcmc = create_mcmc(store_every = 1000)
    )
  )
  expect_equal(
    length(all_experiments),
    length(create_all_experiments()) - 1
  )
})

test_that("should exclude one model, code from article", {

  if (rappdirs::app_dir()$os == "win")  return()

  # This test takes too long
  if (!beastier::is_on_ci()) return()

    generative_experiment <- create_experiment(
      inference_conditions = create_inference_conditions(
        model_type = "generative",
        run_if = "always"
      ),
      inference_model = create_inference_model(
        tree_prior = create_yule_tree_prior(),
        clock_model = create_strict_clock_model(),
        site_model = create_jc69_site_model()
      )
    )

    candidate_experiments <- create_all_experiments(
      exclude_model = generative_experiment$inference_model
    )
    expect_equal(
      length(candidate_experiments),
      length(create_all_experiments()) - 1
    )
})
