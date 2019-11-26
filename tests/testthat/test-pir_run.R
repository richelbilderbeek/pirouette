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

  experiment <- pirouette::create_test_gen_experiment()
  experiments <- list(experiment)

  # Create and bundle the parameters
  pir_params <- pirouette::create_pir_params(
    alignment_params = pirouette::create_test_alignment_params(),
    experiments = experiments
  )

  # Files not yet created
  filenames <- pirouette::get_pir_params_filenames(pir_params)
  testit::assert(all(!file.exists(filenames)))

  # Run pirouette
  errors <- pirouette::pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  # Files created
  testit::assert(all(file.exists(filenames)))

  # Return value all at once
  testthat::expect_silent(pirouette::check_pir_out(errors))

  # Return value
  testthat::expect_true("tree" %in% names(errors))
  testthat::expect_true(is.factor(errors$tree))
  testthat::expect_true("true" %in% errors$tree)

  testthat::expect_true("inference_model" %in% names(errors))
  testthat::expect_true(is.factor(errors$inference_model))
  testthat::expect_true("generative" %in% errors$inference_model)

  testthat::expect_true("inference_model_weight" %in% names(errors))
  testthat::expect_true(is.na(errors$inference_model_weight))
  testthat::expect_true(!is.factor(errors$inference_model_weight))

  testthat::expect_true("site_model" %in% names(errors))
  testthat::expect_true(is.factor(errors$site_model))
  testthat::expect_true("JC69" %in% errors$site_model)

  testthat::expect_true("clock_model" %in% names(errors))
  testthat::expect_true(is.factor(errors$clock_model))
  testthat::expect_true("strict" %in% errors$clock_model)

  testthat::expect_true("tree_prior" %in% names(errors))
  testthat::expect_true(is.factor(errors$tree_prior))
  testthat::expect_true("yule" %in% errors$tree_prior)

  testthat::expect_true("error_1" %in% names(errors))
  testthat::expect_true(!is.factor(errors$error_1))

  # Errors more than zero
  col_first_error <- which(colnames(errors) == "error_1")
  col_last_error <- ncol(errors)
  testthat::expect_true(all(errors[, col_first_error:col_last_error] > 0.0))
  n_errors <- col_last_error - col_first_error + 1
  testthat::expect_true(n_errors < 11) # due to burn-in

})

test_that("short run with unusual logging intervals", {

  if (!beastier::is_on_travis()) return()
  if (!beastier::is_beast2_installed()) return()
  skip("#364")
  phylogeny <- ape::read.tree(text = "((A:2, B:2):1, C:3);")

  experiment <- pirouette::create_test_gen_experiment()
  experiments <- list(experiment)

  # Create and bundle the parameters
  pir_params <- pirouette::create_pir_params(
    alignment_params = pirouette::create_test_alignment_params(),
    experiments = experiments
  )

  # Run pirouette
  expect_silent(
    pir_run(
      phylogeny = phylogeny,
      pir_params = pir_params
    )
  )
})

test_that("nodeSub: true and twin alignments must differ", {

  if (!beastier::is_on_travis()) return()
  if (!beastier::is_beast2_installed()) return()

  phylogeny <- ape::read.tree(text = "((A:2, B:2):1, C:3);")

  alignment_params <- pirouette::create_test_alignment_params(
    sim_true_alignment_fun =
      pirouette::get_sim_true_alignment_with_lns_site_model_fun()
  )
  pirouette::check_alignment_params(alignment_params)

  experiment <- pirouette::create_test_gen_experiment()
  pirouette::check_experiment(experiment)

  experiments <- list(experiment)

  # This is wrong: the twin alignment must follow a JC69 site model,
  # currently it uses the same models as the true alignment
  twinning_params <- pirouette::create_twinning_params(
    sim_twin_alignment_fun =
      pirouette::get_sim_twin_alignment_with_std_site_model_fun()
  )
  pirouette::check_twinning_params(twinning_params)

  # Create and bundle the parameters
  pir_params <- pirouette::create_pir_params(
    alignment_params = alignment_params,
    experiments = experiments,
    twinning_params = twinning_params
  )

  # Run pirouette
  errors <- pirouette::pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  # These alignments should differ
  true_alignment <- readLines(pir_params$alignment_params$fasta_filename)
  twin_alignment <- readLines(
    pir_params$twinning_params$twin_alignment_filename
  )
  testthat::expect_false(all(true_alignment == twin_alignment))

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
  experiment <- pirouette::create_experiment()
  experiment$inference_model$tree_prior <- beautier::create_cbs_tree_prior()
  pir_params <-
    pirouette::create_test_pir_params(experiments = list(experiment))

  testthat::expect_error(
    pirouette::pir_run(
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

  experiment_yule <- pirouette::create_experiment(
    inference_conditions = pirouette::create_inference_conditions(
      model_type = "candidate",
      run_if = "best_candidate",
      do_measure_evidence = TRUE
    ),
    inference_model = beautier::create_inference_model(
      tree_prior = beautier::create_yule_tree_prior(),
      mcmc = beautier::create_mcmc(chain_length = 2000, store_every = 1000)
    ),
    beast2_options = beastier::create_beast2_options(rng_seed = 314),
    est_evidence_mcmc = beautier::create_nested_sampling_mcmc(epsilon = 100.0)
  )
  experiments <- list(experiment_yule)

  pir_params <- pirouette::create_pir_params(
    alignment_params = pirouette::create_test_alignment_params(),
    experiments = experiments
  )

  # Files not yet created
  filenames <- pirouette::get_pir_params_filenames(pir_params)
  testit::assert(all(!file.exists(filenames)))

  errors <- pirouette::pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  # Files created
  testit::assert(all(file.exists(filenames)))

  testthat::expect_true("candidate" %in% errors$inference_model)
  testthat::expect_true(file.exists(pir_params$evidence_filename))
  testthat::expect_true(all(errors$inference_model_weight > 0.0))
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
  experiment <- pirouette::create_test_gen_experiment()
  experiments <- list(experiment)

  twinning_params <- pirouette::create_twinning_params()

  pir_params <- pirouette::create_pir_params(
    alignment_params = pirouette::create_test_alignment_params(),
    experiments = experiments,
    twinning_params = twinning_params
  )

  filenames <- pirouette::get_pir_params_filenames(pir_params)
  testit::assert(all(!file.exists(filenames)))

  errors <- pirouette::pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  # Files created
  testit::assert(all(file.exists(filenames)))

  # Return value
  testthat::expect_true("tree" %in% names(errors))
  testthat::expect_true(is.factor(errors$tree))
  testthat::expect_true("true" %in% errors$tree)
  testthat::expect_true("twin" %in% errors$tree)

  testthat::expect_silent(
    pirouette::pir_to_pics(phylogeny = phylogeny,
      pir_params = pir_params,
      folder = tempdir()
    )
  )
})


test_that("most_evidence, with twinning", {

  if (!beastier::is_on_travis()) return()
  if (!beastier::is_beast2_installed()) return()
  if (!mauricer::is_beast2_ns_pkg_installed()) return()

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
  testit::assert(mauricer::is_beast2_ns_pkg_installed())

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  beast2_options <- create_beast2_options(
    input_filename = beastier::create_temp_input_filename(),
    output_state_filename = beastier::create_temp_state_filename(),
    rng_seed = 314
  )
  errors_filename <- tempfile(pattern = "errors_", fileext = ".csv")

  experiment_yule <- pirouette::create_experiment(
    inference_conditions = pirouette::create_inference_conditions(
      model_type = "candidate",
      run_if = "best_candidate",
      do_measure_evidence = TRUE
    ),
    inference_model = beautier::create_inference_model(
      tree_prior = beautier::create_yule_tree_prior(),
      mcmc = beautier::create_mcmc(chain_length = 3000, store_every = 1000)
    ),
    beast2_options = beast2_options,
    errors_filename = errors_filename,
    est_evidence_mcmc = beautier::create_nested_sampling_mcmc(epsilon = 100.0)
  )
  experiment_bd <- pirouette::create_experiment(
    inference_conditions = pirouette::create_inference_conditions(
      model_type = "candidate",
      run_if = "best_candidate",
      do_measure_evidence = TRUE
    ),
    inference_model = beautier::create_inference_model(
      tree_prior = beautier::create_bd_tree_prior(),
      mcmc = beautier::create_mcmc(chain_length = 3000, store_every = 1000)
    ),
    beast2_options = beast2_options,
    errors_filename = errors_filename,
    est_evidence_mcmc = beautier::create_nested_sampling_mcmc(epsilon = 100.0)
  )
  experiments <- list(experiment_yule, experiment_bd)
  pirouette::check_experiments(experiments)

  pir_params <- pirouette::create_pir_params(
    alignment_params = pirouette::create_test_alignment_params(),
    experiments = experiments,
    twinning_params = pirouette::create_twinning_params()
  )

  filenames <- pirouette::get_pir_params_filenames(pir_params)
  testit::assert(all(!file.exists(filenames)))
  pir_params$verbose <- TRUE

  errors <- pirouette::pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  # Files created
  testit::assert(all(file.exists(filenames)))

  testthat::expect_true("candidate" %in% errors$inference_model)

  testthat::expect_true(all(errors$inference_model_weight > 0.0))

})

test_that("Abuse", {

  testthat::expect_error(
    pirouette::pir_run(
      phylogeny = "nonsense",
      pir_params = pirouette::create_test_pir_params()
    ),
    "'phylogeny' must be a valid phylogeny"
  )

  testthat::expect_error(
    pirouette::pir_run(
      phylogeny = ape::rcoal(2),
      pir_params = "nonsense"
    ),
    "pir_params"
  )

})
