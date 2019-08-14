context("test-est_evidences")

test_that("use, 2 candidates", {

  if (!beastier::is_on_ci()) return()
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

test_that("cleans up", {

  if (!beastier::is_on_ci()) return()
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

  trees_filename_1 <- experiment_1$beast2_options$output_trees_filenames
  trees_filename_2 <- experiment_2$beast2_options$output_trees_filenames
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
  experiments <- list(create_test_cand_experiment())
  evidence_epsilon <- 100.0

  # No BEAST2 installed
  if (!beastier::is_beast2_installed()) {
    expect_error(
      est_evidences(
        fasta_filename = fasta_filename,
        experiments = experiments,
        evidence_epsilon = evidence_epsilon
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
      experiments = experiments,
      evidence_epsilon = evidence_epsilon
    ),
    "'fasta_filename' must be the name of an existing file"
  )

  # experiments
  expect_error(
    est_evidences(
      fasta_filename = fasta_filename,
      experiments = "nonsense",
      evidence_epsilon = evidence_epsilon
    ),
    "'experiments' must be a list of one or more experiments"
  )

  # evidence_epsilon
  expect_error(
    est_evidences(
      fasta_filename = fasta_filename,
      experiments = experiments,
      evidence_epsilon = "nonsense"
    ),
    "'evidence_epsilon' must be one numerical value."
  )

  if (!beastier::is_on_ci()) return()
  if (rappdirs::app_dir()$os == "win") return()
  if (!beastier::is_beast2_installed()) return()

  expect_silent(
    est_evidences(
      fasta_filename = fasta_filename,
      experiments = experiments,
      evidence_epsilon = evidence_epsilon
    )
  )
})
