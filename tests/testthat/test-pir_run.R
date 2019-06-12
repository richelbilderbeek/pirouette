context("test-pir_run")

test_that("generative", {

  if (!beastier::is_on_travis()) return()
  if (!beastier::is_beast2_installed()) return()

  # type       | run_if         | measure  | inference                          # nolint this is no commented code
  #            |                | evidence | model
  # -----------|----------------|----------|-----------
  # generative | always         |FALSE     |Default                             # nolint this is no commented code
  #
  # should result in:
  #
  # tree|inference_model|inference_model_weight|errors                          # nolint this is no commented code
  # ----|---------------|----------------------|-------
  # true|generative     |NA                    |0.1                             # nolint this is no commented code
  #
  # All weights and errors are random, but possibly valid, numbers

  phylogeny <- ape::read.tree(text = "((A:2, B:2):1, C:3);")

  experiment <- create_test_gen_experiment()
  experiments <- list(experiment)

  pir_params <- create_pir_params(
    alignment_params = create_test_alignment_params(),
    experiments = experiments
  )

  # Files not yet created
  filenames <- c(
    pir_params$alignment_params$fasta_filename,
    pir_params$experiments[[1]]$beast2_options$input_filename,
    pir_params$experiments[[1]]$beast2_options$output_log_filename,
    pir_params$experiments[[1]]$beast2_options$output_trees_filenames,
    pir_params$experiments[[1]]$beast2_options$output_state_filename
  )
  testit::assert(all(!file.exists(filenames)))
  # Evidence files will not be created,
  #   as all models have do_measure_evidence == FALSE
  testit::assert(!file.exists(pir_params$evidence_filename))


  errors <- pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  # Files created
  testit::assert(all(file.exists(filenames)))
  # Evidence file will not be created
  testit::assert(!file.exists(pir_params$evidence_filename))

  # Return value all at once
  expect_silent(check_pir_out(errors))

  # Return value
  expect_true("tree" %in% names(errors))
  expect_true(is.factor(errors$tree))
  expect_true("true" %in% errors$tree)

  expect_true("inference_model" %in% names(errors))
  expect_true(is.factor(errors$inference_model))
  expect_true("generative" %in% errors$inference_model)

  expect_true("inference_model_weight" %in% names(errors))
  expect_true(is.na(errors$inference_model_weight))
  expect_true(!is.factor(errors$inference_model_weight))

  expect_true("site_model" %in% names(errors))
  expect_true(is.factor(errors$site_model))
  expect_true("JC69" %in% errors$site_model)

  expect_true("clock_model" %in% names(errors))
  expect_true(is.factor(errors$clock_model))
  expect_true("strict" %in% errors$clock_model)

  expect_true("tree_prior" %in% names(errors))
  expect_true(is.factor(errors$tree_prior))
  expect_true("yule" %in% errors$tree_prior)

  expect_true("error_1" %in% names(errors))
  expect_true(!is.factor(errors$error_1))

  # Errors more than zero
  col_first_error <- which(colnames(errors) == "error_1")
  col_last_error <- ncol(errors)
  expect_true(all(errors[, col_first_error:col_last_error] > 0.0))
  n_errors <- col_last_error - col_first_error + 1
  expect_true(n_errors < 11) # due to burn-in

  # Error files created
  expect_true(
    file.exists(
      pir_params$experiments[[1]]$errors_filename
    )
  )
})

test_that("abuse: generative, CBS with too few taxa", {

  # https://github.com/richelbilderbeek/pirouette/issues/153
  #
  # For n_taxa < 5, the Coalalescent Bayesian Skyline plot throws an exception
  # This should be handled gracefully

  # type       | run_if         | measure  | inference                          # nolint this is no commented code
  #            |                | evidence | model
  # -----------|----------------|----------|-----------
  # generative | always         |FALSE     |CBS                                 # nolint this is no commented code
  #
  # should result in an error

  # Select all experiments with 'run_if' is 'always'
  experiment <- create_experiment()
  experiment$inference_model$tree_prior <- create_cbs_tree_prior()
  pir_params <- create_test_pir_params(experiments = list(experiment))

  expect_error(
    pir_run(
      phylogeny = ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);"),
      pir_params = pir_params
    ),
    "Too few taxa to use a Coalescent Bayesian Skyline tree prior"
  )
})


test_that("most_evidence, one candidate", {

  if (!beastier::is_on_travis()) return()
  if (!beastier::is_beast2_installed()) return()

  # type       | run_if         | measure  | inference                          # nolint this is no commented code
  #            |                | evidence | model
  # -----------|----------------|----------|-----------
  # candidate  | best_candidate |TRUE      |Yule                                # nolint this is no commented code
  #
  # should result in:
  #
  # tree|inference_model|inference_model_weight|errors                          # nolint this is no commented code
  # ----|---------------|----------------------|-------
  # true|candidate      |1.0                   |0.1                             # nolint this is no commented code
  #
  # as only the best candidate is run.
  #
  # All weights and errors are random, but possibly valid, numbers

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")

  experiment_yule <- create_experiment(
    inference_conditions = create_inference_conditions(
      model_type = "candidate",
      run_if = "best_candidate",
      do_measure_evidence = TRUE
    ),
    inference_model = create_inference_model(
      tree_prior = create_yule_tree_prior(),
      mcmc = create_mcmc(chain_length = 2000, store_every = 1000)
    ),
    beast2_options = create_beast2_options(rng_seed = 314),
    est_evidence_mcmc = create_nested_sampling_mcmc(epsilon = 100.0)
  )
  experiments <- list(experiment_yule)

  pir_params <- create_pir_params(
    alignment_params = create_test_alignment_params(),
    experiments = experiments
  )

  # Files not yet created
  filenames <- c(
    pir_params$alignment_params$fasta_filename,
    pir_params$evidence_filename,
    pir_params$experiments[[1]]$beast2_options$input_filename,
    pir_params$experiments[[1]]$beast2_options$output_log_filename,
    pir_params$experiments[[1]]$beast2_options$output_trees_filenames,
    pir_params$experiments[[1]]$beast2_options$output_state_filename
  )
  testit::assert(all(!file.exists(filenames)))

  errors <- pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  # Files created
  testit::assert(all(file.exists(filenames)))

  expect_true("candidate" %in% errors$inference_model)
  expect_true(file.exists(pir_params$evidence_filename))

  expect_true(all(errors$inference_model_weight > 0.0))
})

test_that("generative with twin", {

  if (!beastier::is_on_travis()) return()
  if (!beastier::is_beast2_installed()) return()

  # type       | run_if         | measure  | inference                          # nolint this is no commented code
  #            |                | evidence | model
  # -----------|----------------|----------|-----------
  # generative | always         |FALSE     |Default                             # nolint this is no commented code
  #
  # should result in:
  #
  # tree|inference_model|inference_model_weight|errors                          # nolint this is no commented code
  # ----|---------------|----------------------|-------
  # true|generative     |NA                    |0.1                             # nolint this is no commented code
  # twin|generative     |NA                    |0.1                             # nolint this is no commented code
  #
  # All weights and errors are random, but possibly valid, numbers

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")

  # Select all experiments with 'run_if' is 'always'
  experiment <- create_experiment(
    inference_conditions = create_inference_conditions(
      model_type = "generative",
      run_if = "always",
      do_measure_evidence = FALSE
    ),
    inference_model = create_inference_model(
      mcmc = create_mcmc(chain_length = 3000, store_every = 1000)
    )
  )
  experiments <- list(experiment)

  twinning_params <- create_twinning_params()

  pir_params <- create_pir_params(
    alignment_params = create_test_alignment_params(),
    experiments = experiments,
    twinning_params = twinning_params
  )

  filenames <- c(
    pir_params$alignment_params$fasta_filename,
    pir_params$experiments[[1]]$beast2_options$input_filename,
    pir_params$experiments[[1]]$beast2_options$output_log_filename,
    pir_params$experiments[[1]]$beast2_options$output_trees_filenames,
    pir_params$experiments[[1]]$beast2_options$output_state_filename,
    to_twin_filename(
      pir_params$experiments[[1]]$beast2_options$input_filename
    ),
    to_twin_filename(
      pir_params$experiments[[1]]$beast2_options$output_log_filename
    ),
    to_twin_filename(
      pir_params$experiments[[1]]$beast2_options$output_trees_filenames
    ),
    to_twin_filename(
      pir_params$experiments[[1]]$beast2_options$output_state_filename
    ),
    pir_params$twinning_params$twin_tree_filename,
    pir_params$twinning_params$twin_alignment_filename
  )
  testit::assert(all(!file.exists(filenames)))
  # Evidence files will not be created,
  #   as all models have do_measure_evidence == FALSE
  testit::assert(!file.exists(pir_params$evidence_filename))
  testit::assert(!file.exists(twinning_params$twin_evidence_filename))

  errors <- pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  # Files created
  testit::assert(all(file.exists(filenames)))
  # Evidence files will not be created
  testit::assert(!file.exists(pir_params$evidence_filename))
  testit::assert(!file.exists(twinning_params$twin_evidence_filename))

  # Return value
  expect_true("tree" %in% names(errors))
  expect_true(is.factor(errors$tree))
  expect_true("true" %in% errors$tree)
  expect_true("twin" %in% errors$tree)
})


test_that("most_evidence, with twinning", {

  if (!beastier::is_on_travis()) return()
  if (!beastier::is_beast2_installed()) return()
  if (!mauricer::is_beast2_pkg_installed("NS")) return()

  # type       | run_if         | measure  | inference                          # nolint this is no commented code
  #            |                | evidence | model
  # -----------|----------------|----------|-----------
  # candidate  | best_candidate |TRUE      |Yule                                # nolint this is no commented code
  # candidate  | best_candidate |TRUE      |Birth-Death                         # nolint this is no commented code
  #
  # should result in:
  #
  # tree|inference_model|inference_model_weight|errors                          # nolint this is no commented code
  # ----|---------------|----------------------|-------
  # true|candidate      |0.6                   |0.2                             # nolint this is no commented code
  # twin|candidate      |0.7                   |0.1                             # nolint this is no commented code
  #
  # as only the best candidate and its twin are run.
  #
  # All weights and errors are random, but possibly valid, numbers

  testit::assert(beastier::is_beast2_installed())
  testit::assert(mauricer::is_beast2_pkg_installed("NS"))

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  beast2_options <- create_beast2_options(rng_seed = 314)

  experiment_yule <- create_experiment(
    inference_conditions = create_inference_conditions(
      model_type = "candidate",
      run_if = "best_candidate",
      do_measure_evidence = TRUE
    ),
    inference_model = create_inference_model(
      tree_prior = create_yule_tree_prior(),
      mcmc = create_mcmc(chain_length = 3000, store_every = 1000)
    ),
    beast2_options = beast2_options,
    est_evidence_mcmc = create_nested_sampling_mcmc(epsilon = 100.0)
  )
  experiment_bd <- create_experiment(
    inference_conditions = create_inference_conditions(
      model_type = "candidate",
      run_if = "best_candidate",
      do_measure_evidence = TRUE
    ),
    inference_model = create_inference_model(
      tree_prior = create_bd_tree_prior(),
      mcmc = create_mcmc(chain_length = 3000, store_every = 1000)
    ),
    beast2_options = beast2_options,
    est_evidence_mcmc = create_nested_sampling_mcmc(epsilon = 100.0)
  )
  experiments <- list(experiment_yule, experiment_bd)

  pir_params <- create_pir_params(
    alignment_params = create_test_alignment_params(),
    experiments = experiments,
    twinning_params = create_twinning_params()
  )

  filenames <- c(
    pir_params$alignment_params$fasta_filename,
    pir_params$evidence_filename,
    pir_params$experiments[[1]]$beast2_options$input_filename,
    pir_params$experiments[[1]]$beast2_options$output_log_filename,
    pir_params$experiments[[1]]$beast2_options$output_trees_filenames,
    pir_params$experiments[[1]]$beast2_options$output_state_filename,
    to_twin_filename(
      pir_params$experiments[[1]]$beast2_options$input_filename
    ),
    to_twin_filename(
      pir_params$experiments[[1]]$beast2_options$output_log_filename
    ),
    to_twin_filename(
      pir_params$experiments[[1]]$beast2_options$output_trees_filenames
    ),
    to_twin_filename(
      pir_params$experiments[[1]]$beast2_options$output_state_filename
    ),
    pir_params$twinning_params$twin_tree_filename,
    pir_params$twinning_params$twin_alignment_filename,
    pir_params$twinning_params$twin_evidence_filename
  )
  testit::assert(all(!file.exists(filenames)))

  errors <- pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  # Files created
  testit::assert(all(file.exists(filenames)))

  expect_true("candidate" %in% errors$inference_model)

  expect_true(all(errors$inference_model_weight > 0.0))
})

test_that("Abuse", {

  expect_error(
    pir_run(
      phylogeny = "nonsense",
      pir_params = create_test_pir_params()
    ),
    "'phylogeny' must be of class 'phylo'"
  )
})
