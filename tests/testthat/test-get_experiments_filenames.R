test_that("use", {

  experiments <- list(
    create_test_gen_experiment(),
    create_test_cand_experiment()
  )

  filenames <- get_experiments_filenames(experiments)
  for (experiment in experiments) {
    expect_true(experiment$beast2_options$input_filename %in% filenames)
    expect_true(experiment$beast2_options$output_log_filename %in% filenames)
    expect_true(experiment$beast2_options$output_trees_filenames %in% filenames)
    expect_true(experiment$beast2_options$output_state_filename %in% filenames)
    expect_true(experiment$errors_filename %in% filenames)
  }
})
