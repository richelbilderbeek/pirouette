test_that("use", {

  experiments <- list(
    create_test_gen_experiment()
  )

  filenames <- get_experiments_filenames(experiments)
  for (experiment in experiments) {
    expect_true(experiment$beast2_options$input_filename %in% filenames)
    expect_true(experiment$inference_model$mcmc$tracelog$filename %in% filenames)
    expect_true(experiment$beast2_options$output_trees_filenames %in% filenames)
    expect_true(experiment$beast2_options$output_state_filename %in% filenames)
    expect_true(experiment$errors_filename %in% filenames)
  }
})

test_that("use, gen and cand", {

  if (rappdirs::app_dir()$os == "win") return()

  experiments <- list(
    create_test_gen_experiment(),
    create_test_cand_experiment()
  )
  # Must use different inference model than generative model
  experiments[[2]]$inference_model$site_model <-
    beautier::create_gtr_site_model()

  filenames <- get_experiments_filenames(experiments)
  for (experiment in experiments) {
    expect_true(experiment$beast2_options$input_filename %in% filenames)
    expect_true(experiment$inference_model$mcmc$tracelog$filename %in% filenames)
    expect_true(experiment$beast2_options$output_trees_filenames %in% filenames)
    expect_true(experiment$beast2_options$output_state_filename %in% filenames)
    expect_true(experiment$errors_filename %in% filenames)
  }
})
