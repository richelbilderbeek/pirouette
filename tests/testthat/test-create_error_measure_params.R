context("test-create_error_measure_params")

test_that("use", {
  expect_silent(create_error_measure_params())
})

test_that("errors are stored correctly", {

  if (!beastier::is_beast2_installed()) return()

  phylogeny <- ape::read.tree(text = "((A:1, B:1):1, (C:1, D:1):1);")

  alignment_params <- pirouette::create_alignment_params(
    root_sequence = pirouette::create_blocked_dna(length = 100),
    mutation_rate = create_standard_mutation_rate,
    rng_seed = 1
  )
  errors_filename <- tempfile(fileext = ".csv")
  experiment <- create_experiment(
    inference_conditions = create_inference_conditions(
      model_type = "generative",
      run_if = "always",
      do_measure_evidence = FALSE # Set to TRUE if UNIX
    ),
    inference_model = create_inference_model(
      tree_prior = create_bd_tree_prior(),
      mcmc = create_mcmc(chain_length = 3000, store_every = 1000)
    ),
    est_evidence_mcmc = create_nested_sampling_mcmc(epsilon = 100.0),
    beast2_options = create_beast2_options(rng_seed = 1),
    errors_filename = errors_filename
  )
  error_measure_params <- create_error_measure_params()
  pir_params <- create_pir_params(
    alignment_params = alignment_params,
    experiments = list(experiment),
    error_measure_params = error_measure_params
  )

  expect_true(
    length(
      list.files(dirname(errors_filename), pattern = basename(errors_filename))
    ) == 0
  )

  df <- pirouette::pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  expect_true(
    length(
      list.files(dirname(errors_filename), pattern = basename(errors_filename))
    ) > 0
  )

})

test_that("abuse", {

  # Exact error messages checked by 'check_error_measure_params]
  expect_error(
    create_error_measure_params(
      rng_seed = "nonsense"
    )
  )
  expect_error(
    create_error_measure_params(
      twin_tree_filename = NA
    )
  )
})
