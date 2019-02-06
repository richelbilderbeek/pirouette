context("test-check_pir_params")

test_that("use", {

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- create_alignment_params(
    root_sequence = create_mono_nuc_dna(length = 4),
    mutation_rate = 0.01
  )
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
    model_select_params = as.list(seq(1, 314)), # To be removed, #90
    inference_params = inference_params
  )

  # OK
  expect_silent(
    check_pir_params(
      pir_params
    )
  )

  # Wrong alignment_params
  pir_params_2 <- pir_params
  pir_params_2$alignment_params <- "pippobaudo"
  expect_error(
    check_pir_params(
      pir_params_2
    )
  )

  # Wrong inference_params
  pir_params_2 <- pir_params
  pir_params_2$inference_params <- "pippobaudo"
  expect_error(
    check_pir_params(
      pir_params_2
    )
  )

  # Wrong model_select_params
  pir_params_2 <- pir_params
  pir_params_2$model_select_params <- "pippobaudo"
  expect_error(
    check_pir_params(
      pir_params_2
    )
  )

  # Wrong error_measure_params
  pir_params_2 <- pir_params
  pir_params_2$error_measure_params <- "pippobaudo"
  expect_error(
    check_pir_params(
      pir_params_2
    )
  )

  # Wrong experiments
  pir_params_2 <- pir_params
  pir_params_2$experiments <- "pippobaudo"
  expect_error(
    check_pir_params(
      pir_params_2
    )
  )

  # Wrong evidence_epsilon
  pir_params_2 <- pir_params
  pir_params_2$evidence_epsilon <- "pippobaudo"
  expect_error(
    check_pir_params(
      pir_params_2
    )
  )
  pir_params_2 <- pir_params
  pir_params_2$evidence_epsilon <- 123456789
  expect_error(
    check_pir_params(
      pir_params_2
    )
  )

  # Wrong evidence_filename
  pir_params_2 <- pir_params
  pir_params_2$evidence_filename <- 123456789
  expect_error(
    check_pir_params(
      pir_params_2
    )
  )
  pir_params_2 <- pir_params
  pir_params_2$evidence_filename <- "pippobaudo"
  expect_error(
    check_pir_params(
      pir_params_2
    )
  )
})
