context("test-pir_run")

test_that("generative", {

  if (!beastier::is_on_travis()) return()

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

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")

  # Select all experiments with 'run_if' is 'always'
  experiment <- create_experiment(
    model_type = "generative",
    run_if = "always",
    do_measure_evidence = FALSE,
    inference_model = create_inference_model(
      mcmc = create_mcmc(chain_length = 2000, store_every = 1000)
    ),
    beast2_options = create_beast2_options(rng_seed = 314)
  )
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
})

test_that("generative, using gamma statistic", {

  if (!beastier::is_on_travis()) return()

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

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")

  # Select all experiments with 'run_if' is 'always'
  experiment <- create_experiment(
    model_type = "generative",
    run_if = "always",
    do_measure_evidence = FALSE,
    inference_model = create_inference_model(
      mcmc = create_mcmc(chain_length = 3000, store_every = 1000)
    )
  )
  experiments <- list(experiment)


  pir_params <- create_pir_params(
    alignment_params = create_test_alignment_params(),
    experiments = experiments,
    error_measure_params = create_error_measure_params(
      burn_in_fraction = 0.0,
      error_function = get_gamma_error_function()
    )
  )
  errors <- pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  testthat::expect_true(is.na(errors$inference_model_weight))
  testthat::expect_true(!is.na(errors$error_1))
  testthat::expect_true(errors$error_1 >= 0.0)
})

test_that("generative, with MRCA prior", {

  if (!beastier::is_on_travis()) return()

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

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")

  # Select all experiments with 'run_if' is 'always'
  experiment <- create_experiment(
    model_type = "generative",
    run_if = "always",
    do_measure_evidence = FALSE,
    inference_model = create_inference_model(
      mcmc = create_mcmc(chain_length = 3000, store_every = 1000)
    )
  )
  experiments <- list(experiment)


  pir_params <- create_pir_params(
    alignment_params = create_test_alignment_params(),
    experiments = experiments,
    error_measure_params = create_error_measure_params(
      burn_in_fraction = 0.0,
      error_function = get_gamma_error_function()
    )
  )
  errors <- pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  testthat::expect_true(is.na(errors$inference_model_weight))
  testthat::expect_true(!is.na(errors$error_1))
  testthat::expect_true(errors$error_1 >= 0.0)
})

test_that("most_evidence, one candidate", {

  if (!beastier::is_on_travis()) return()

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
    model_type = "candidate",
    run_if = "best_candidate",
    do_measure_evidence = TRUE,
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

test_that("most_evidence, two candidates", {

  if (!beastier::is_on_travis()) return()

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
  # true|candidate      |0.6                   |0.1                             # nolint this is no commented code
  #
  # as only the best candidate is run.
  #
  # All weights and errors are random, but possibly valid, numbers

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")

  experiment_yule <- create_experiment(
    model_type = "candidate",
    run_if = "best_candidate",
    do_measure_evidence = TRUE,
    inference_model = create_inference_model(
      tree_prior = create_yule_tree_prior(),
      mcmc = create_mcmc(chain_length = 2000, store_every = 1000)
    ),
    beast2_options = create_beast2_options(rng_seed = 314),
    est_evidence_mcmc = create_nested_sampling_mcmc(epsilon = 100.0)
  )
  experiment_bd <- experiment_yule
  experiment_bd$inference_model$tree_prior <- create_bd_tree_prior()
  experiments <- list(experiment_yule, experiment_bd)

  pir_params <- create_pir_params(
    alignment_params = create_test_alignment_params(),
    experiments = experiments,
    verbose = TRUE
  )


  # Files not yet created
  filenames <- c(
    pir_params$alignment_params$fasta_filename,
    pir_params$evidence_filename,
    pir_params$experiments[[1]]$beast2_options$input_filename,
    pir_params$experiments[[1]]$beast2_options$output_log_filename,
    pir_params$experiments[[1]]$beast2_options$output_trees_filenames,
    pir_params$experiments[[1]]$beast2_options$output_state_filename,
    pir_params$experiments[[2]]$beast2_options$input_filename,
    pir_params$experiments[[2]]$beast2_options$output_log_filename,
    pir_params$experiments[[2]]$beast2_options$output_trees_filenames,
    pir_params$experiments[[2]]$beast2_options$output_state_filename
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

test_that("generative and most_evidence, generative not in most_evidence", {

  if (!beastier::is_on_travis()) return()

  # type       | run_if         | measure  | inference                          # nolint this is no commented code
  #            |                | evidence | model
  # -----------|----------------|----------|-----------
  # generative | always         |FALSE     |Yule                                # nolint this is no commented code
  # candidate  | best_candidate |TRUE      |Birth-Death                         # nolint this is no commented code
  #
  # should result in:
  #
  # tree|inference_model|inference_model_weight|errors                          # nolint this is no commented code
  # ----|---------------|----------------------|-------
  # true|generative     |NA                    |0.1                             # nolint this is no commented code
  # true|candidate      |1.0                   |0.2                             # nolint this is no commented code
  #
  # as also the best (and only) candidate is run
  #
  # All weights and errors are random, but possibly valid, numbers

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")

  experiment_generative <- create_experiment(
    model_type = "generative",
    run_if = "always",
    do_measure_evidence = FALSE,
    inference_model = create_inference_model(
      tree_prior = create_yule_tree_prior(),
      mcmc = create_mcmc(chain_length = 3000, store_every = 1000)
    ),
    beast2_options = create_beast2_options(rng_seed = 314),
    est_evidence_mcmc = create_nested_sampling_mcmc(epsilon = 100.0)
  )
  experiment_bd <- experiment_generative
  experiment_bd$model_type <- "candidate"
  experiment_bd$run_if <- "best_candidate"
  experiment_bd$do_measure_evidence <- TRUE
  experiment_bd$inference_model$tree_prior <- create_bd_tree_prior()
  experiments <- list(experiment_generative, experiment_bd)


  pir_params <- create_pir_params(
    alignment_params = create_alignment_params(
      root_sequence = create_blocked_dna(length = 16),
      mutation_rate = 0.1
    ),
    experiments = experiments,
    verbose = TRUE
  )
  errors <- pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )


  expect_true("generative" %in% errors$inference_model)
  expect_true("candidate" %in% errors$inference_model)
  expect_true(is.na(errors$inference_model_weight[1]))
  expect_true(all(errors$error_1 >= 0.0))
  expect_false(is.na(errors$inference_model_weight[2]))
})

test_that("generative and most_evidence, generative in most_evidence", {

  if (!beastier::is_on_travis()) return()

  # type       | run_if         | measure  | inference                          # nolint this is no commented code
  #            |                | evidence | model
  # -----------|----------------|----------|-----------
  # generative | always         |TRUE      |Yule                                # nolint this is no commented code
  # candidate  | best_candidate |TRUE      |Birth-Death                         # nolint this is no commented code
  #
  # should result in:
  #
  # tree|inference_model|inference_model_weight|errors                          # nolint this is no commented code
  # ----|---------------|----------------------|-------
  # true|generative     |0.6                   |0.1                             # nolint this is no commented code
  # true|candidate      |0.4                   |0.2                             # nolint this is no commented code
  #
  # as also the best (and only) candidate is run. The candidate is also
  # run if its model weight is less than the generative model.
  #
  # All weights and errors are random, but possibly valid, numbers

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")

  experiment_generative <- create_experiment(
    model_type = "generative",
    run_if = "always",
    do_measure_evidence = TRUE,
    inference_model = create_inference_model(
      tree_prior = create_yule_tree_prior(),
      mcmc = create_mcmc(chain_length = 2000, store_every = 1000)
    ),
    est_evidence_mcmc = create_nested_sampling_mcmc(epsilon = 100.0),
    beast2_options = create_beast2_options(rng_seed = 314)
  )
  experiment_bd <- experiment_generative
  experiment_bd$model_type <- "candidate"
  experiment_bd$run_if <- "best_candidate"
  experiment_bd$do_measure_evidence <- TRUE
  experiments <- list(experiment_generative, experiment_bd)


  pir_params <- create_pir_params(
    alignment_params = create_test_alignment_params(),
    experiments = experiments
  )

  errors <- pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  expect_true(all(errors$error_1 >= 0.0))
  expect_true(all(errors$inference_model_weight >= 0.0))
  expect_true("generative" %in% errors$inference_model)
  expect_true("candidate" %in% errors$inference_model)
})

test_that("two candidates, run both", {

  if (!beastier::is_on_travis()) return()

  # type       | run_if         | measure  | inference                          # nolint this is no commented code
  #            |                | evidence | model
  # -----------|----------------|----------|-----------
  # candidate  | always         |TRUE      |Yule                                # nolint this is no commented code
  # candidate  | always         |TRUE      |Birth-Death                         # nolint this is no commented code
  #
  # should result in:
  #
  # tree|inference_model|inference_model_weight|errors                          # nolint this is no commented code
  # ----|---------------|----------------------|-------
  # true|candidate      |0.6                   |0.2                             # nolint this is no commented code
  # true|candidate      |0.5                   |0.1                             # nolint this is no commented code
  #
  # as only the best candidate is run.
  #
  # All weights and errors are random, but possibly valid, numbers

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")

  experiment_yule <- create_experiment(
    model_type = "candidate",
    run_if = "always",
    do_measure_evidence = TRUE,
    inference_model = create_inference_model(
      tree_prior = create_yule_tree_prior(),
      mcmc = create_mcmc(chain_length = 2000, store_every = 1000)
    ),
    est_evidence_mcmc = create_nested_sampling_mcmc(epsilon = 100.0),
    beast2_options = create_beast2_options(rng_seed = 314)
  )
  experiment_bd <- experiment_yule
  experiments <- list(experiment_yule, experiment_bd)

  pir_params <- create_pir_params(
    alignment_params = create_test_alignment_params(),
    experiments = experiments,
    verbose = TRUE
  )

  # Files not yet created
  filenames <- c(
    pir_params$alignment_params$fasta_filename,
    pir_params$evidence_filename,
    pir_params$experiments[[1]]$beast2_options$input_filename,
    pir_params$experiments[[1]]$beast2_options$output_log_filename,
    pir_params$experiments[[1]]$beast2_options$output_trees_filenames,
    pir_params$experiments[[1]]$beast2_options$output_state_filename,
    pir_params$experiments[[2]]$beast2_options$input_filename,
    pir_params$experiments[[2]]$beast2_options$output_log_filename,
    pir_params$experiments[[2]]$beast2_options$output_trees_filenames,
    pir_params$experiments[[2]]$beast2_options$output_state_filename
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


test_that("most_evidence, three candidates", {

  if (!beastier::is_on_travis()) return()

  # type       | run_if         | measure  | inference                          # nolint this is no commented code
  #            |                | evidence | model
  # -----------|----------------|----------|-----------
  # candidate  | best_candidate |TRUE      |JC69                                # nolint this is no commented code
  # candidate  | best_candidate |TRUE      |HKY                                 # nolint this is no commented code
  # candidate  | best_candidate |TRUE      |TN93                                # nolint this is no commented code
  #
  # should result in:
  #
  # tree|inference_model|inference_model_weight|errors                          # nolint this is no commented code
  # ----|---------------|----------------------|-------
  # true|candidate      |0.6                   |0.1                             # nolint this is no commented code
  #
  # as only the best candidate is run.
  #
  # All weights and errors are random, but possibly valid, numbers
  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")

  experiment_jc69 <- create_experiment(
    model_type = "candidate",
    run_if = "best_candidate",
    do_measure_evidence = TRUE,
    inference_model = create_inference_model(
      site_model = create_jc69_site_model(),
      mcmc = create_mcmc(chain_length = 4000, store_every = 1000)
    ),
    beast2_options = create_beast2_options(rng_seed = 314),
    est_evidence_mcmc = create_nested_sampling_mcmc(epsilon = 100.0)
  )
  experiment_hky <- experiment_jc69
  experiment_hky$inference_model$site_model <- create_hky_site_model()

  experiment_tn93 <- experiment_jc69
  experiment_tn93$inference_model$site_model <- create_tn93_site_model()
  experiments <- list(experiment_jc69, experiment_hky, experiment_tn93)

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


test_that("most_evidence, four candidates", {

  if (!beastier::is_on_travis()) return()

  # type       | run_if         | measure  | inference                          # nolint this is no commented code
  #            |                | evidence | model
  # -----------|----------------|----------|-----------
  # candidate  | best_candidate |TRUE      |JC69                                # nolint this is no commented code
  # candidate  | best_candidate |TRUE      |HKY                                 # nolint this is no commented code
  # candidate  | best_candidate |TRUE      |TN93                                # nolint this is no commented code
  # candidate  | best_candidate |TRUE      |GTR                                 # nolint this is no commented code
  #
  # should result in:
  #
  # tree|inference_model|inference_model_weight|errors                          # nolint this is no commented code
  # ----|---------------|----------------------|-------
  # true|candidate      |0.6                   |0.1                             # nolint this is no commented code
  #
  # as only the best candidate is run.
  #
  # All weights and errors are random, but possibly valid, numbers

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")

  experiment_jc69 <- create_experiment(
    model_type = "candidate",
    run_if = "best_candidate",
    do_measure_evidence = TRUE,
    inference_model = create_inference_model(
      site_model = create_jc69_site_model(),
      mcmc = create_mcmc(chain_length = 4000, store_every = 1000)
    ),
    beast2_options = create_beast2_options(rng_seed = 314),
    est_evidence_mcmc = create_nested_sampling_mcmc(epsilon = 100.0)
  )
  experiment_hky <- experiment_jc69
  experiment_hky$inference_model$site_model <- create_hky_site_model()
  experiment_tn93 <- experiment_jc69
  experiment_tn93$inference_model$site_model <- create_tn93_site_model()
  experiment_gtr <- experiment_jc69
  experiment_gtr$inference_model$site_model <- create_gtr_site_model()
  experiments <- list(
    experiment_jc69, experiment_hky, experiment_tn93, experiment_gtr
  )

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
    model_type = "generative",
    run_if = "always",
    do_measure_evidence = FALSE,
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

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  beast2_options <- create_beast2_options(rng_seed = 314)

  experiment_yule <- create_experiment(
    model_type = "candidate",
    run_if = "best_candidate",
    do_measure_evidence = TRUE,
    inference_model = create_inference_model(
      tree_prior = create_yule_tree_prior(),
      mcmc = create_mcmc(chain_length = 3000, store_every = 1000)
    ),
    beast2_options = beast2_options,
    est_evidence_mcmc = create_nested_sampling_mcmc(epsilon = 100.0)
  )
  experiment_bd <- create_experiment(
    model_type = "candidate",
    run_if = "best_candidate",
    do_measure_evidence = TRUE,
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

test_that("twin parameters", {

  if (!beastier::is_on_travis()) return()

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")

  # Select all experiments with 'run_if' is 'always'
  experiment <- create_experiment(
    model_type = "generative",
    run_if = "always",
    do_measure_evidence = FALSE,
    inference_model = create_inference_model(
      mcmc = create_mcmc(chain_length = 3000, store_every = 1000)
    )
  )
  experiments <- list(experiment)

  pir_params <- create_pir_params(
    alignment_params = create_test_alignment_params(),
    experiments = experiments,
    twinning_params = create_twinning_params()
  )

  # Files not yet created
  filenames <- c(
    pir_params$experiments[[1]]$beast2_options$input_filename,
    pir_params$experiments[[1]]$beast2_options$output_log_filename,
    pir_params$experiments[[1]]$beast2_options$output_trees_filenames,
    pir_params$experiments[[1]]$beast2_options$output_state_filename
  )
  twin_filenames <- c(
    to_twin_filename(filenames)
  )
  testit::assert(all(!file.exists(filenames)))
  testit::assert(all(!file.exists(twin_filenames)))

  errors <- pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  # Files created
  testit::assert(all(file.exists(filenames)))
  testit::assert(all(file.exists(twin_filenames)))

})
