test_that("use, gen", {

  experiment <- create_test_gen_experiment()
  filenames <- get_experiment_filenames(experiment)
  expect_true(experiment$beast2_options$input_filename %in% filenames)
  expect_true(experiment$inference_model$mcmc$tracelog$filename %in% filenames)
  expect_true(experiment$beast2_options$output_trees_filenames %in% filenames)
  expect_true(experiment$beast2_options$output_state_filename %in% filenames)
  expect_true(experiment$errors_filename %in% filenames)
})

test_that("use, cand", {

  if (rappdirs::app_dir()$os == "win") return()

  experiment <- create_test_cand_experiment()
  filenames <- get_experiment_filenames(experiment)
  expect_true(experiment$beast2_options$input_filename %in% filenames)
  expect_true(experiment$inference_model$mcmc$tracelog$filename %in% filenames)
  expect_true(experiment$beast2_options$output_trees_filenames %in% filenames)
  expect_true(experiment$beast2_options$output_state_filename %in% filenames)
  expect_true(experiment$errors_filename %in% filenames)
})
