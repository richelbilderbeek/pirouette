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
  inference_param <- create_inference_param(
    mcmc = beautier::create_mcmc(chain_length = 2000, store_every = 1000)
  )
  error_measure_params <- create_error_measure_params()
  expect_silent(
    pir_run_check_inputs(
      phylogeny = phylogeny,
      alignment_params = alignment_params,
      model_select_params = model_select_params,
      inference_param = inference_param,
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
  inference_param <- create_inference_param(
    mcmc = beautier::create_mcmc(chain_length = 2000, store_every = 1000)
  )
  error_measure_params <- create_error_measure_params()

  # Exact error messages checked by 'pir_run_check_inputs'
  expect_error(
    pir_run_check_inputs(
      phylogeny = phylogeny,
      alignment_params = model_select_params,
      model_select_params = model_select_params,
      inference_param = inference_param,
      error_measure_params = error_measure_params
    )
  )
  expect_error(
    pir_run_check_inputs(
      phylogeny = phylogeny,
      alignment_params = alignment_params,
      model_select_params = model_select_params,
      inference_param = alignment_params,
      error_measure_params = error_measure_params
    )
  )
  expect_error(
    pir_run_check_inputs(
      phylogeny = phylogeny,
      alignment_params = alignment_params,
      model_select_params = alignment_params,
      inference_param = inference_param,
      error_measure_params = error_measure_params
    )
  )
  expect_error(
    pir_run_check_inputs(
      phylogeny = phylogeny,
      alignment_params = alignment_params,
      model_select_params = model_select_params,
      inference_param = inference_param,
      error_measure_params = inference_param
    )
  )
})
