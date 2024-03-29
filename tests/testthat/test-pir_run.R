test_that("generative", {

  if (!beautier::is_on_github_actions()) return()
  if (!beastier::is_beast2_installed()) return()

  skip("Takes too long 9")

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

  pir_params <- create_test_pir_params()

  # Run pirouette
  pir_out <- pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  # Return value all at once
  expect_silent(check_pir_out(pir_out))

  # Return value
  expect_true("tree" %in% names(pir_out))
  expect_true(is.factor(pir_out$tree))
  expect_true("true" %in% pir_out$tree)

  expect_true("inference_model" %in% names(pir_out))
  expect_true(is.factor(pir_out$inference_model))
  expect_true("generative" %in% pir_out$inference_model)

  expect_true("inference_model_weight" %in% names(pir_out))
  expect_true(is.na(pir_out$inference_model_weight))
  expect_true(!is.factor(pir_out$inference_model_weight))

  expect_true("site_model" %in% names(pir_out))
  expect_true(is.factor(pir_out$site_model))
  expect_true("JC69" %in% pir_out$site_model)

  expect_true("clock_model" %in% names(pir_out))
  expect_true(is.factor(pir_out$clock_model))
  expect_true("strict" %in% pir_out$clock_model)

  expect_true("tree_prior" %in% names(pir_out))
  expect_true(is.factor(pir_out$tree_prior))
  expect_true("yule" %in% pir_out$tree_prior)

  expect_true("error_1" %in% names(pir_out))
  expect_true(!is.factor(pir_out$error_1))

  # Errors more than zero
  col_first_error <- which(colnames(pir_out) == "error_1")
  col_last_error <- ncol(pir_out)
  expect_true(all(pir_out[, col_first_error:col_last_error] > 0.0))
  n_errors <- col_last_error - col_first_error + 1
  expect_true(n_errors < 11) # due to burn-in

  # Redundant check
  expect_silent(check_pir_out(pir_out))
})

test_that("short run with unusual logging intervals", {

  if (!beautier::is_on_github_actions()) return()
  if (!beastier::is_beast2_installed()) return()

  skip("Takes too long 10")

  phylogeny <- ape::read.tree(text = "((A:2, B:2):1, C:3);")

  experiment <- create_test_gen_experiment()
  experiments <- list(experiment)

  # Create and bundle the parameters
  pir_params <- create_pir_params(
    alignment_params = create_test_alignment_params(),
    experiments = experiments
  )

  testthat::expect_equal(
    2000,
    pir_params$experiments[[1]]$inference_model$mcmc$chain_length
  )
  pir_params$experiments[[1]]$inference_model$mcmc$tracelog$log_every <- 300
  pir_params$experiments[[1]]$inference_model$mcmc$treelog$log_every <- 500

  # Run pirouette
  expect_silent(
    df <- pir_run(
      phylogeny = phylogeny,
      pir_params = pir_params
    )
  )
})

test_that("nodeSub: true and twin alignments must differ", {

  if (!beautier::is_on_github_actions()) return()
  if (!beastier::is_beast2_installed()) return()

  skip("Takes too long 11")

  phylogeny <- ape::read.tree(text = "((A:2, B:2):1, C:3);")

  alignment_params <- create_test_alignment_params(
    sim_tral_fun =
      get_sim_tral_with_lns_nsm_fun()
  )
  check_alignment_params(alignment_params)

  experiment <- create_test_gen_experiment()
  check_experiment(experiment)

  experiments <- list(experiment)

  # This is wrong: the twin alignment must follow a JC69 site model,
  # currently it uses the same models as the true alignment
  twinning_params <- create_twinning_params(
    sim_twal_fun =
      get_sim_twal_with_std_nsm_fun()
  )
  check_twinning_params(twinning_params)

  # Create and bundle the parameters
  pir_params <- create_pir_params(
    alignment_params = alignment_params,
    experiments = experiments,
    twinning_params = twinning_params
  )

  # Run pirouette
  errors <- pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  # These alignments should differ
  true_alignment <- readLines(pir_params$alignment_params$fasta_filename)
  twin_alignment <- readLines(
    pir_params$twinning_params$twin_alignment_filename
  )
  expect_false(all(true_alignment == twin_alignment))

})

test_that("abuse: generative, CBS with too few taxa", {

  if (!beastier::is_beast2_installed()) return()

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
  experiment$inference_model$tree_prior <- beautier::create_cbs_tree_prior()
  pir_params <-
    create_test_pir_params(experiments = list(experiment))

  expect_error(
    pir_run(
      phylogeny = ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);"),
      pir_params = pir_params
    ),
    "Too few taxa to use a Coalescent Bayesian Skyline tree prior"
  )

})


test_that("most_evidence, one candidate", {

  if (!beautier::is_on_github_actions()) return()
  if (!beastier::is_beast2_installed()) return()

  skip("Takes too long 12")

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
  experiment_yule <- create_test_cand_experiment()
  experiments <- list(experiment_yule)

  pir_params <- create_pir_params(
    alignment_params = create_test_alignment_params(),
    experiments = experiments,
    evidence_filename = get_temp_evidence_filename()
  )

  # Files not yet created
  filenames <- get_pir_params_filenames(pir_params)
  testthat::expect_true(all(!file.exists(filenames)))

  errors <- pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  expect_true("candidate" %in% errors$inference_model)
  expect_true(file.exists(pir_params$evidence_filename))
  expect_true(all(errors$inference_model_weight > 0.0))
})

test_that("generative with twin", {

  if (!beautier::is_on_github_actions()) return()
  if (!beastier::is_beast2_installed()) return()

  skip("Takes too long 13")

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
  experiment <- create_test_gen_experiment()
  experiments <- list(experiment)

  twinning_params <- create_twinning_params()

  pir_params <- create_pir_params(
    alignment_params = create_test_alignment_params(),
    experiments = experiments,
    twinning_params = twinning_params
  )

  filenames <- get_pir_params_filenames(pir_params)
  testthat::expect_true(all(!file.exists(filenames)))

  errors <- pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  # Return value
  expect_true("tree" %in% names(errors))
  expect_true(is.factor(errors$tree))
  expect_true("true" %in% errors$tree)
  expect_true("twin" %in% errors$tree)

  expect_silent(
    pir_to_pics(phylogeny = phylogeny,
      pir_params = pir_params,
      folder = tempdir()
    )
  )
})


test_that("most_evidence, with twinning", {

  if (!beautier::is_on_github_actions()) return()
  if (!mcbette::can_run_mcbette()) return()

  skip("Takes too long 14")

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

  testthat::expect_true(beastier::is_beast2_installed())
  testthat::expect_true(mauricer::is_beast2_ns_pkg_installed())

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  beast2_options <- create_beast2_options(
    input_filename = beastier::create_temp_input_filename(),
    output_state_filename = beastier::create_temp_state_filename(),
    rng_seed = 314
  )
  errors_filename <- tempfile(pattern = "errors_", fileext = ".csv")

  experiment_yule <- create_test_cand_experiment()
  experiment_bd <- create_test_cand_experiment(
    inference_model = beautier::create_inference_model(
      tree_prior = beautier::create_bd_tree_prior(),
    )
  )
  experiment_yule$beast2_options <- experiment_bd$beast2_options
  experiment_yule$inference_model$mcmc <- experiment_bd$inference_model$mcmc
  experiment_yule$errors_filename <- experiment_bd$errors_filename

  experiments <- list(experiment_yule, experiment_bd)
  check_experiments(experiments)

  pir_params <- create_pir_params(
    alignment_params = create_test_alignment_params(),
    experiments = experiments,
    twinning_params = create_twinning_params(
      twin_evidence_filename = get_temp_evidence_filename()
    ),
    evidence_filename = get_temp_evidence_filename()
  )
  filenames <- get_pir_params_filenames(pir_params)
  testthat::expect_true(all(!file.exists(filenames)))
  pir_params$verbose <- TRUE

  errors <- pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  expect_true("candidate" %in% errors$inference_model)

  expect_true(all(errors$inference_model_weight > 0.0))

})

test_that("Abuse", {

  expect_error(
    pir_run(
      phylogeny = "nonsense",
      pir_params = create_test_pir_params()
    ),
    "'phylogeny' must be a valid phylogeny"
  )

  expect_error(
    pir_run(
      phylogeny = ape::rcoal(2),
      pir_params = "nonsense"
    ),
    "pir_params"
  )

})
