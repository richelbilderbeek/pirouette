context("test-create_all_experiments")

test_that("use", {

  if (rappdirs::app_dir()$os == "win") return()
  all_experiments <- create_all_experiments()
  for (i in seq_along(all_experiments)) {
    experiment <- all_experiments[[i]]
    expect_silent(check_experiment(experiment))
  }
})

test_that("it can exclude an experiment", {

  if (rappdirs::app_dir()$os == "win")  return()

  all_experiments <- create_all_experiments(
    exclude_model = create_inference_model(
      site_model = create_site_model_jc69(),
      clock_model = create_strict_clock_model(),
      tree_prior = create_tree_prior_bd(),
      mcmc = create_mcmc(store_every = 1000)
    )
  )
  expect_true(length(all_experiments) == (length(create_all_experiments()) - 1))
})
