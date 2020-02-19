test_that("use, gen", {

  experiment <- create_test_gen_experiment()
  filenames <- get_experiment_filenames(experiment)

  if (1 + 1 == 2) {
    flat_experiment <- unlist(experiment)
    filename_indices <- stringr::str_detect(
      string = names(flat_experiment),
      pattern = "filename"
    )
    filenames_alt <- na.omit(as.character(unlist(flat_experiment[filename_indices])))
    expect_equal(
      length(filenames_alt),
      length(filenames)
    )
  }

  expect_true(experiment$beast2_options$input_filename %in% filenames)
  expect_true(experiment$inference_model$mcmc$tracelog$filename %in% filenames)
  expect_true(experiment$inference_model$mcmc$treelog$filename %in% filenames)
  expect_true(experiment$beast2_options$output_state_filename %in% filenames)
  expect_true(experiment$errors_filename %in% filenames)
})

test_that("use, cand", {

  if (rappdirs::app_dir()$os == "win") return()

  experiment <- create_test_cand_experiment()
  filenames <- get_experiment_filenames(experiment)
  expect_true(experiment$beast2_options$input_filename %in% filenames)
  expect_true(experiment$inference_model$mcmc$tracelog$filename %in% filenames)
  expect_true(experiment$inference_model$mcmc$treelog$filename %in% filenames)
  expect_true(experiment$beast2_options$output_state_filename %in% filenames)
  expect_true(experiment$errors_filename %in% filenames)
})
