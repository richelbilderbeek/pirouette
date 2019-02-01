context("test-pir_run_check_inputs")

test_that("use", {
  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- create_alignment_params(
    root_sequence = "acgt",
    mutation_rate = 0.01
  )
  model_select_params <- list_model_select_params(
    create_gen_model_select_param(
      alignment_params = alignment_params,
      tree_prior = beautier::create_bd_tree_prior()
    )
  )
  inference_params <- create_inference_params(
    mcmc = beautier::create_mcmc(chain_length = 2000, store_every = 1000)
  )
  experiments <- list(create_experiment())
  error_measure_params <- create_error_measure_params()
  expect_silent(
    pir_run_check_inputs(
      phylogeny = phylogeny,
      alignment_params = alignment_params,
      model_select_params = model_select_params,
      inference_params = inference_params,
      experiments = experiments,
      error_measure_params = error_measure_params
    )
  )
})

test_that("abuse", {

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- create_alignment_params(
    root_sequence = "acgt",
    mutation_rate = 0.01
  )
  model_select_params <- list_model_select_params(
    create_gen_model_select_param(
      alignment_params = alignment_params,
      tree_prior = beautier::create_bd_tree_prior()
    )
  )
  inference_params <- create_inference_params(
    mcmc = beautier::create_mcmc(chain_length = 2000, store_every = 1000)
  )
  experiments <- list(create_experiment())
  error_measure_params <- create_error_measure_params()

  # Exact error messages checked by 'pir_run_check_inputs'
  expect_error(
    pir_run_check_inputs(
      phylogeny = phylogeny,
      alignment_params = "nonsense",
      model_select_params = model_select_params,
      inference_params = inference_params,
      experiments = experiments,
      error_measure_params = error_measure_params
    ),
    "'alignment_params'"
  )
  expect_error(
    pir_run_check_inputs(
      phylogeny = phylogeny,
      alignment_params = alignment_params,
      model_select_params = "nonsense",
      inference_params = inference_params,
      experiments = experiments,
      error_measure_params = error_measure_params
    )
  )
  expect_error(
    pir_run_check_inputs(
      phylogeny = phylogeny,
      alignment_params = alignment_params,
      model_select_params = model_select_params,
      inference_params = "nonsense",
      experiments = experiments,
      error_measure_params = error_measure_params
    )
  )
  expect_error(
    pir_run_check_inputs(
      phylogeny = phylogeny,
      alignment_params = alignment_params,
      model_select_params = model_select_params,
      inference_params = inference_params,
      experiments = "nonsense",
      error_measure_params = error_measure_params
    ),
    "'experiments'"
  )
  expect_error(
    pir_run_check_inputs(
      phylogeny = phylogeny,
      alignment_params = alignment_params,
      model_select_params = model_select_params,
      inference_params = inference_params,
      experiments = experiments,
      error_measure_params = "nonsense"
    ),
    "'error_measure_params'"
  )
  expect_error(
    pir_run_check_inputs(
      phylogeny = "nonsense",
      alignment_params = alignment_params,
      model_select_params = model_select_params,
      inference_params = inference_params,
      experiments = experiments,
      error_measure_params = error_measure_params
    ),
    "'phylogeny' must be of class 'phylo'"
  )
})
