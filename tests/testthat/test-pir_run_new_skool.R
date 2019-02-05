context("test-pir_run_new_skool")

test_that("generative only", {

  if (!beastier::is_on_travis()) return()

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- create_alignment_params(
    mutation_rate = 0.01
  )
  file.remove(alignment_params$fasta_filename)

  # Select all experiments with 'run_if' is 'always'
  experiment <- create_experiment(
    inference_model = create_inference_model(
      mcmc = create_mcmc(chain_length = 3000, store_every = 1000)
    )
  )
  experiment$run_if <- "always"
  experiments <- list(experiment)
  check_experiments(experiments)

  pir_params <- create_pir_params(
    alignment_params = alignment_params,
    model_select_params = as.list(seq(1, 314)),
    inference_params = create_inference_params(
      mcmc = beautier::create_mcmc(chain_length = 10000, store_every = 1000)
    ),
    experiments = experiments
  )
  errors <- pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  # Files created
  testit::assert(file.exists(alignment_params$fasta_filename))
  testit::assert(file.exists(errors$beast2_input_filename))
  testit::assert(file.exists(errors$beast2_output_log_filename))
  testit::assert(file.exists(errors$beast2_output_trees_filename))
  testit::assert(file.exists(errors$beast2_output_state_filename))

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

test_that("most_evidence", {

  if (!beastier::is_on_travis()) return()


  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- create_alignment_params(
    root_sequence = "acgt",
    mutation_rate = 0.01
  )
  experiment_yule <- create_experiment(
    model_type = "candidate",
    run_if = "best_candidate",
    do_measure_evidence = TRUE,
    inference_model = create_inference_model(
      tree_prior = create_yule_tree_prior(),
      mcmc = create_mcmc(chain_length = 3000, store_every = 1000)
    ),
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
    est_evidence_mcmc = create_nested_sampling_mcmc(epsilon = 100.0)
  )
  experiments <- list(experiment_yule, experiment_bd)


  pir_params <- create_pir_params(
    alignment_params = alignment_params,
    model_select_params = as.list(seq(1, 314)),
    experiments = experiments
  )

  # TODO: fix warning
  errors <- pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  expect_true("most_evidence" %in% errors$inference_model)
  expect_true(all(errors$inference_model_weight > 0.0))

  # Errors more than zero
  col_first_error <- which(colnames(errors) == "error_1")
  col_last_error <- ncol(errors)
  expect_true(all(errors[, col_first_error:col_last_error] > 0.0))

  skip("Issue 69, #69")
  expect_true(file.exists(pir_params$evidence_filename))
})

test_that("generative and most_evidence, generative not in most_evidence", {

  if (!beastier::is_on_travis()) return()
  skip("Issue 69, #69")

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- create_alignment_params(mutation_rate = 0.01)
  model_select_params <- list(
    create_gen_model_select_param(
      alignment_params = alignment_params
    ),
    create_best_model_select_param(
      site_models = beautier::create_site_models()[4],
      clock_models = beautier::create_clock_models()[2],
      tree_priors = beautier::create_tree_priors()[5]
    )
  )
  inference_params <- create_inference_params(
    mcmc = beautier::create_mcmc(chain_length = 2000, store_every = 1000)
  )

  pir_params <- create_pir_params(
    alignment_params = alignment_params,
    model_select_params = model_select_params,
    inference_params = inference_params
  )
  errors <- pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  expect_true("most_evidence" %in% errors$inference_model)
  expect_true(is.na(errors$inference_model_weight[1]))
  expect_true(is.numeric(errors$inference_model_weight[2]))

  # Errors more than zero
  col_first_error <- which(colnames(errors) == "error_1")
  col_last_error <- ncol(errors)
  expect_true(all(errors[, col_first_error:col_last_error] > 0.0))
})

test_that("generative and most_evidence, generative in most_evidence", {

  if (!beastier::is_on_travis()) return()
  skip("Issue 69, #69")

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- create_alignment_params(mutation_rate = 0.01)
  model_select_params <- list(
    create_gen_model_select_param(
      alignment_params = alignment_params
    ),
    create_best_model_select_param(
      site_models = list(alignment_params$site_model),
      clock_models = list(alignment_params$clock_model),
      tree_priors = list(beautier::create_bd_tree_prior())
    )
  )
  inference_params <- create_inference_params(
    mcmc = beautier::create_mcmc(chain_length = 2000, store_every = 1000)
  )

  pir_params <- create_pir_params(
    alignment_params = alignment_params,
    model_select_params = model_select_params,
    inference_params = inference_params
  )
  errors <- pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  expect_true("most_evidence" %in% errors$inference_model)
  expect_true(is.numeric(errors$inference_model_weight[1]))

  # Errors more than zero
  col_first_error <- which(colnames(errors) == "error_1")
  col_last_error <- ncol(errors)
  expect_true(all(errors[, col_first_error:col_last_error] > 0.0))

})

test_that("generative with twin", {

  if (!beastier::is_on_travis()) return()
  skip("Issue 69, #69")

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- create_alignment_params(
    mutation_rate = 0.01
  )
  twinning_params <- create_twinning_params()
  model_select_params <- create_gen_model_select_param(
    alignment_params = alignment_params
  )
  inference_params <- create_inference_params(
    mcmc = beautier::create_mcmc(chain_length = 2000, store_every = 1000)
  )

  # Remove files, just to be sure
  file.remove(twinning_params$twin_tree_filename)
  file.remove(twinning_params$twin_alignment_filename)

  pir_params <- create_pir_params(
    alignment_params = alignment_params,
    model_select_params = model_select_params,
    inference_params = inference_params,
    twinning_params = twinning_params
  )
  errors <- pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  # Files created
  testit::assert(file.exists(twinning_params$twin_tree_filename))
  testit::assert(file.exists(twinning_params$twin_alignment_filename))

  # Return value
  expect_true("tree" %in% names(errors))
  expect_true(is.factor(errors$tree))
  expect_true("true" %in% errors$tree)
  expect_true("twin" %in% errors$tree)
})
