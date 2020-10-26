test_that("use", {
  if (!beastier::is_beast2_installed()) return()

  beast2_options <- beastier::create_beast2_options()

  # Will delete nothing, as file is absent
  expect_true(!file.exists(beast2_options$output_state_filename))
  delete_beast2_state_files(
    beast2_optionses = list(beast2_options)
  )

  babette::bbt_run_from_model(
    fasta_filename = babette::get_babette_path("anthus_aco_sub.fas"),
    inference_model = beautier::create_test_inference_model(),
    beast2_options = beast2_options
  )

  expect_true(file.exists(beast2_options$output_state_filename))

  suppressMessages(
    delete_beast2_state_files(
      beast2_optionses = list(beast2_options),
      verbose = TRUE
    )
  )

  expect_true(!file.exists(beast2_options$output_state_filename))
})
