context("test-est_evidences")

test_that("use", {

  if (!beastier::is_on_ci()) return()
  if (rappdirs::app_dir()$os == "win") return()
  if (!beastier::is_beast2_installed()) return()

  # Create an alignment
  fasta_filename <- tempfile(fileext = ".fasta")
  create_alignment_file(
    phylogeny = ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);"),
    alignment_params = create_alignment_params(
      root_sequence = "acgt",
      mutation_rate = 0.1,
      fasta_filename = fasta_filename
    )
  )
  testit::assert(file.exists(fasta_filename))

  # Setup experiments
  experiment_1 <- create_experiment(
    inference_conditions = create_inference_conditions(
      model_type = "candidate",
      run_if = "best_candidate",
      do_measure_evidence = TRUE
    ),
    inference_model = beautier::create_inference_model(
      site_model = beautier::create_jc69_site_model(),
      clock_model = beautier::create_strict_clock_model(),
      tree_prior = beautier::create_yule_tree_prior(),
      mcmc = create_mcmc(chain_length = 3000, store_every = 1000)
    ),
    beast2_options = beastier::create_beast2_options(
      beast2_path = beastier::get_default_beast2_bin_path()
    ),
    est_evidence_mcmc = create_mcmc_nested_sampling(epsilon = 100.0)
  )
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

  # Create an alignment
  fasta_filename <- tempfile(fileext = ".fasta")
  create_alignment_file(
    phylogeny = ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);"),
    alignment_params = create_alignment_params(
      root_sequence = "acgt",
      mutation_rate = 0.1,
      fasta_filename = fasta_filename
    )
  )
  testit::assert(file.exists(fasta_filename))

  # Setup experiments
  experiment_1 <- create_experiment(
    inference_conditions = create_inference_conditions(
      model_type = "candidate",
      run_if = "best_candidate",
      do_measure_evidence = TRUE
    ),
    inference_model = beautier::create_inference_model(
      site_model = beautier::create_jc69_site_model(),
      clock_model = beautier::create_strict_clock_model(),
      tree_prior = beautier::create_yule_tree_prior(),
      mcmc = create_mcmc(chain_length = 3000, store_every = 1000)
    ),
    beast2_options = beastier::create_beast2_options(
      beast2_path = beastier::get_default_beast2_bin_path()
    ),
    est_evidence_mcmc = create_mcmc_nested_sampling(epsilon = 100.0)
  )
  experiment_2 <- experiment_1
  experiment_2$inference_model$site_model <- beautier::create_hky_site_model()
  experiment_2$beast2_options <- beastier::create_beast2_options()
  experiments <- list(experiment_1, experiment_2)
  check_experiments(experiments)

  trees_filename_1 <- experiment_1$beast2_options$output_trees_filenames
  trees_filename_2 <- experiment_2$beast2_options$output_trees_filenames
  expect_false(trees_filename_1 == trees_filename_2)
  expect_true(all(!file.exists(c(trees_filename_1, trees_filename_2))))

  df <- est_evidences(
    fasta_filename = fasta_filename,
    experiments = experiments
  )

  # Still no files exist, as they are deleted by est_evidences
  expect_true(all(!file.exists(c(trees_filename_1, trees_filename_2))))
})


test_that("abuse", {

  if (rappdirs::app_dir()$os == "win") return()

  fasta_filename <- system.file(
    "extdata",
    "alignment.fas",
    package = "pirouette"
  )
  testit::assert(file.exists(fasta_filename))
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
