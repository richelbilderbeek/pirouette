test_that("use, 2 candidates", {

  if (!beautier::is_on_ci()) return()
  if (rappdirs::app_dir()$os == "win") return()
  if (!beastier::is_beast2_installed()) return()

  fasta_filename <- system.file(
    "extdata", "test_output_3.fas", package = "pirouette"
  )
  experiment_1 <- create_test_cand_experiment()
  experiment_2 <- experiment_1
  experiment_2$inference_model$site_model <- beautier::create_hky_site_model()
  experiments <- list(experiment_1, experiment_2)

  df <- est_evidences(
    fasta_filename = fasta_filename,
    experiments = experiments
  )
  expect_true(all(!is.na(df$marg_log_lik)))
  expect_true(all(!is.na(df$weight)))
  expect_true(all(df$weight >= 0.0))
  expect_true(all(df$weight <= 1.0))
})

test_that("use, 1 candidate", {

  if (!beautier::is_on_ci()) return()
  if (rappdirs::app_dir()$os == "win") return()
  if (!beastier::is_beast2_installed()) return()

  fasta_filename <- system.file(
    "extdata", "test_output_3.fas", package = "pirouette"
  )
  experiment <- create_test_cand_experiment()
  experiments <- list(experiment)

  df <- est_evidences(
    fasta_filename = fasta_filename,
    experiments = experiments
  )
  expect_true("ess" %in% colnames(df))
  expect_true(!is.na(df$marg_log_lik))
  expect_true(!is.na(df$marg_log_lik_sd))
  expect_true(!is.na(df$weight))
  expect_true(df$weight == 1.0)
})

test_that("use, 1 candidate, CBS tree prior that should give error", {

  if (!beautier::is_on_ci()) return()
  if (rappdirs::app_dir()$os == "win") return()
  if (!beastier::is_beast2_installed()) return()

  fasta_filename <- system.file(
    "extdata", "test_output_3.fas", package = "pirouette"
  )
  experiment <- create_test_cand_experiment()
  experiment$inference_model$tree_prior <- create_cbs_tree_prior()
  experiments <- list(experiment)

  expect_error(
    est_evidences(
      fasta_filename = fasta_filename,
      experiments = experiments
    ),
    "'group_sizes_dimension' .* must be less than the number of taxa"
  )
})

test_that("cleans up", {

  if (!beautier::is_on_ci()) return()
  if (rappdirs::app_dir()$os == "win") return()
  if (!beastier::is_beast2_installed()) return()

  fasta_filename <- system.file(
    "extdata", "test_output_3.fas", package = "pirouette"
  )
  experiment_1 <- create_test_cand_experiment()
  experiment_2 <- experiment_1
  experiment_2$inference_model$site_model <- beautier::create_hky_site_model()
  experiments <- list(experiment_1, experiment_2)
  check_experiments(experiments)

  trees_filename_1 <- experiment_1$inference_model$mcmc$treelog$filename
  trees_filename_2 <- experiment_2$inference_model$mcmc$treelog$filename
  expect_true(trees_filename_1 == trees_filename_2)

  # These are regular BEAST2 run files. A marginal likelihood estimation
  # is a different kind of run and stores its BEAST2 files in a
  # temporary location after which these files are deleted
  expect_true(!file.exists(trees_filename_1))

  df <- est_evidences(
    fasta_filename = fasta_filename,
    experiments = experiments
  )

  # Still no files exist, as they are deleted by est_evidences
  expect_true(!file.exists(trees_filename_1))
})


test_that("abuse", {

  if (rappdirs::app_dir()$os == "win") return()

  fasta_filename <- system.file(
    "extdata",
    "alignment.fas",
    package = "pirouette"
  )
  beautier::check_file_exists(fasta_filename, "fasta_filename")
  experiments <- list(
    create_test_cand_experiment()
  )


  # No BEAST2 installed
  if (!beastier::is_beast2_installed()) {
    expect_error(
      est_evidences(
        fasta_filename = fasta_filename,
        experiments = experiments
      ),
      "BEAST2 not installed"
    )
    return()
  }
  testit::assert(beastier::is_beast2_installed())

  # fasta_filename
  expect_error(
    est_evidences(
      fasta_filename = "nonsense",
      experiments = experiments
    ),
    "File 'fasta_filename' not found. Could not find file with path 'nonsense'"
  )

  # experiments
  expect_error(
    est_evidences(
      fasta_filename = fasta_filename,
      experiments = "nonsense"
    ),
    "'experiments' must be a list of one or more experiments"
  )

  if (!beautier::is_on_ci()) return()
  if (rappdirs::app_dir()$os == "win") return()
  if (!beastier::is_beast2_installed()) return()

  expect_silent(
    est_evidences(
      fasta_filename = fasta_filename,
      experiments = experiments
    )
  )
})
