test_that("use, no twinning, no evidence estimation", {

  pir_params <- create_test_pir_params()

  filenames <- get_pir_params_filenames(pir_params)
  expect_true(pir_params$alignment_params$fasta_filename %in% filenames)
  for (experiment in pir_params$experiments) {
    expect_true(experiment$beast2_options$input_filename %in% filenames)
    expect_true(experiment$inference_model$mcmc$tracelog$filename %in% filenames)
    expect_true(experiment$beast2_options$output_trees_filenames %in% filenames)
    expect_true(experiment$beast2_options$output_state_filename %in% filenames)
    expect_true(experiment$errors_filename %in% filenames)

    # Nope, evidence is never estimated ...
    testit::assert(!experiment$inference_conditions$do_measure_evidence)
  }
  # Evidence is never estimated, thus no evidence file
  expect_false(pir_params$evidence_filename %in% filenames)
})

test_that("use, no twinning, evidence estimation", {

  pir_params <- create_test_pir_params()

  # We measure the evidence of at least one experiment
  pir_params$experiments[[1]]$inference_conditions$do_measure_evidence <- TRUE

  filenames <- get_pir_params_filenames(pir_params)
  expect_true(pir_params$alignment_params$fasta_filename %in% filenames)
  for (experiment in pir_params$experiments) {
    expect_true(experiment$beast2_options$input_filename %in% filenames)
    expect_true(experiment$inference_model$mcmc$tracelog$filename %in% filenames)
    expect_true(experiment$beast2_options$output_trees_filenames %in% filenames)
    expect_true(experiment$beast2_options$output_state_filename %in% filenames)
    expect_true(experiment$errors_filename %in% filenames)
  }
  # Therefore, this file will be present
  expect_true(pir_params$evidence_filename %in% filenames)
})

test_that("use, twinning, no evidence estimation", {

  pir_params <- create_test_pir_params(
    twinning_params = create_twinning_params()
  )

  filenames <- get_pir_params_filenames(pir_params)
  expect_true(pir_params$alignment_params$fasta_filename %in% filenames)
  for (experiment in pir_params$experiments) {
    # True
    expect_true(experiment$beast2_options$input_filename %in% filenames)
    expect_true(experiment$inference_model$mcmc$tracelog$filename %in% filenames)
    expect_true(experiment$beast2_options$output_trees_filenames %in% filenames)
    expect_true(experiment$beast2_options$output_state_filename %in% filenames)
    expect_true(experiment$errors_filename %in% filenames)
    # Twin
    expect_true(to_twin_filename(
      experiment$beast2_options$input_filename) %in% filenames)
    expect_true(to_twin_filename(
      experiment$inference_model$mcmc$tracelog$filename) %in% filenames)
    expect_true(to_twin_filename(
      experiment$beast2_options$output_trees_filenames) %in% filenames)
    expect_true(to_twin_filename(
      experiment$beast2_options$output_state_filename) %in% filenames)
    expect_true(to_twin_filename(experiment$errors_filename) %in% filenames)

    # Nope, evidence is never estimated ...
    testit::assert(!experiment$inference_conditions$do_measure_evidence)

  }
  expect_true(pir_params$twinning_params$twin_tree_filename %in% filenames)
  expect_true(pir_params$twinning_params$twin_alignment_filename %in% filenames)

  # Evidence is never estimated, thus no evidence files
  expect_false(pir_params$evidence_filename %in% filenames)
  expect_false(pir_params$twinning_params$twin_evidence_filename %in% filenames)
})

test_that("use, twinning, evidence estimation", {

  pir_params <- create_test_pir_params(
    twinning_params = create_twinning_params()
  )
  # We measure the evidence of at least one experiment
  pir_params$experiments[[1]]$inference_conditions$do_measure_evidence <- TRUE

  filenames <- get_pir_params_filenames(pir_params)
  expect_true(pir_params$alignment_params$fasta_filename %in% filenames)
  for (experiment in pir_params$experiments) {
    # True
    expect_true(experiment$beast2_options$input_filename %in% filenames)
    expect_true(experiment$inference_model$mcmc$tracelog$filename %in% filenames)
    expect_true(experiment$beast2_options$output_trees_filenames %in% filenames)
    expect_true(experiment$beast2_options$output_state_filename %in% filenames)
    expect_true(experiment$errors_filename %in% filenames)
    # Twin
    expect_true(to_twin_filename(
      experiment$beast2_options$input_filename) %in% filenames)
    expect_true(to_twin_filename(
      experiment$inference_model$mcmc$tracelog$filename) %in% filenames)
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
