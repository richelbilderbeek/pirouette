test_that("use, no twinning", {

  pir_params <- create_test_pir_params()

  filenames <- get_pir_params_filenames(pir_params)
  expect_true(pir_params$alignment_params$fasta_filename %in% filenames)
  for (experiment in pir_params$experiments) {
    expect_true(experiment$beast2_options$input_filename %in% filenames)
    expect_true(experiment$beast2_options$output_log_filename %in% filenames)
    expect_true(experiment$beast2_options$output_trees_filenames %in% filenames)
    expect_true(experiment$beast2_options$output_state_filename %in% filenames)
    expect_true(experiment$errors_filename %in% filenames)
  }
  expect_true(pir_params$evidence_filename %in% filenames)
})

test_that("use, twinning", {

  pir_params <- create_test_pir_params(
    twinning_params = create_twinning_params()
  )

  filenames <- get_pir_params_filenames(pir_params)
  expect_true(pir_params$alignment_params$fasta_filename %in% filenames)
  for (experiment in pir_params$experiments) {
    # True
    expect_true(experiment$beast2_options$input_filename %in% filenames)
    expect_true(experiment$beast2_options$output_log_filename %in% filenames)
    expect_true(experiment$beast2_options$output_trees_filenames %in% filenames)
    expect_true(experiment$beast2_options$output_state_filename %in% filenames)
    expect_true(experiment$errors_filename %in% filenames)
    # Twin
    expect_true(to_twin_filename(
      experiment$beast2_options$input_filename) %in% filenames)
    expect_true(to_twin_filename(
      experiment$beast2_options$output_log_filename) %in% filenames)
    expect_true(to_twin_filename(
      experiment$beast2_options$output_trees_filenames) %in% filenames)
    expect_true(to_twin_filename(
      experiment$beast2_options$output_state_filename) %in% filenames)
    expect_true(to_twin_filename(experiment$errors_filename) %in% filenames)

  }
  expect_true(pir_params$evidence_filename %in% filenames)
  expect_true(pir_params$twinning_params$twin_tree_filename %in% filenames)
  expect_true(pir_params$twinning_params$twin_alignment_filename %in% filenames)
  expect_true(pir_params$twinning_params$twin_evidence_filename %in% filenames)
})
