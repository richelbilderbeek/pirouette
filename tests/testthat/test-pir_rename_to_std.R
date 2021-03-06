test_that("use, gen only", {

  skip("Takes too long 2")

  pir_params <- create_test_pir_params_setup(
    has_candidate = FALSE,
    has_twinning = FALSE
  )
  folder_name <- tempfile("pir_rename_to_std_1_")
  pir_params <- pir_rename_to_std(
    pir_params,
    folder_name = folder_name
  )

  expect_silent(check_pir_params(pir_params))
  expect_equal(
    pir_params$alignment_params$fasta_filename,
    file.path(folder_name, "alignment.fas")
  )
  expect_equal(
    pir_params$experiments[[1]]$beast2_options$input_filename,
      file.path(folder_name, "gen.xml")
  )
  expect_equal(
    pir_params$experiments[[1]]$beast2_options$output_state_filename,
      file.path(folder_name, "gen.xml.state")
  )
  expect_equal(
    pir_params$experiments[[1]]$errors_filename,
      file.path(folder_name, "gen_errors.csv")
  )
  expect_equal(
    pir_params$experiments[[1]]$inference_model$mcmc$tracelog$filename,
      file.path(folder_name, "gen.log")
  )
  expect_equal(
    pir_params$experiments[[1]]$inference_model$mcmc$screenlog$filename,
      file.path(folder_name, "gen.csv")
  )
  expect_equal(
    pir_params$experiments[[1]]$inference_model$mcmc$treelog$filename,
      file.path(folder_name, "gen.trees")
  )
  expect_equal(
    pir_params$experiments[[1]]$est_evidence_mcmc$tracelog$filename,
      file.path(folder_name, "gen_evidence.log")
  )
  expect_equal(
    pir_params$experiments[[1]]$est_evidence_mcmc$screenlog$filename,
      file.path(folder_name, "gen_evidence.csv")
  )
  expect_equal(
    pir_params$experiments[[1]]$est_evidence_mcmc$treelog$filename,
      file.path(folder_name, "gen_evidence.trees")
  )
})

test_that("use, gen + cand", {

  skip("Takes too long 3")

  if (rappdirs::app_dir()$os == "win") return()

  pir_params <- create_test_pir_params_setup(
    has_candidate = TRUE,
    has_twinning = FALSE
  )
  folder_name <- tempfile("pir_rename_to_std_2_")
  pir_params <- pir_rename_to_std(
    pir_params,
    folder_name = folder_name
  )

  expect_silent(check_pir_params(pir_params))

  # Best
  expect_equal(
    pir_params$experiments[[2]]$beast2_options$input_filename,
      file.path(folder_name, "best.xml")
  )
  expect_equal(
    pir_params$experiments[[2]]$beast2_options$output_state_filename,
      file.path(folder_name, "best.xml.state")
  )
  expect_equal(
    pir_params$experiments[[2]]$errors_filename,
      file.path(folder_name, "best_errors.csv")
  )
  expect_equal(
    pir_params$experiments[[2]]$inference_model$mcmc$tracelog$filename,
      file.path(folder_name, "best.log")
  )
  expect_equal(
    pir_params$experiments[[2]]$inference_model$mcmc$screenlog$filename,
      file.path(folder_name, "best.csv")
  )
  expect_equal(
    pir_params$experiments[[2]]$inference_model$mcmc$treelog$filename,
      file.path(folder_name, "best.trees")
  )
  expect_equal(
    pir_params$experiments[[2]]$est_evidence_mcmc$tracelog$filename,
      file.path(folder_name, "best_evidence.log")
  )
  expect_equal(
    pir_params$experiments[[2]]$est_evidence_mcmc$screenlog$filename,
      file.path(folder_name, "best_evidence.csv")
  )
  expect_equal(
    pir_params$experiments[[2]]$est_evidence_mcmc$treelog$filename,
      file.path(folder_name, "best_evidence.trees")
  )

})

test_that("use, gen + twin", {

  skip("Takes too long 4")

  pir_params <- create_test_pir_params_setup(
    has_candidate = FALSE,
    has_twinning = TRUE
  )
  folder_name <- tempfile("pir_rename_to_std_3_")
  pir_params <- pir_rename_to_std(
    pir_params,
    folder_name = folder_name
  )
  get_pir_params_filenames(pir_params)
  expect_silent(check_pir_params(pir_params))
  expect_true(beautier::is_one_na(pir_params$evidence_filename))
  expect_true(
    beautier::is_one_na(pir_params$twinning_params$twin_evidence_filename)
  )
  # Twin
  expect_equal(
    pir_params$twinning_params$twin_tree_filename,
    file.path(folder_name, "twin.newick")
  )
  expect_equal(
    pir_params$twinning_params$twin_alignment_filename,
    file.path(folder_name, "alignment_twin.fas")
  )
})

test_that("use, gen + cand + twin", {

  skip("Takes too long 5")

  if (rappdirs::app_dir()$os == "win") return()

  pir_params <- create_test_pir_params_setup(
    has_candidate = TRUE,
    has_twinning = TRUE
  )
  folder_name <- tempfile("pir_rename_to_std_4_")
  pir_params <- pir_rename_to_std(
    pir_params,
    folder_name = folder_name
  )
  get_pir_params_filenames(pir_params)
  expect_silent(check_pir_params(pir_params))

  # Evidences
  expect_equal(
    pir_params$evidence_filename,
    file.path(folder_name, "evidence.csv")
  )
  expect_equal(
    pir_params$twinning_params$twin_evidence_filename,
    file.path(folder_name, "evidence_twin.csv")
  )
})
