context("test-check_pir_out")

test_that("simulated data", {
  if (!beastier::is_on_travis()) return()
  if (!beastier::is_beast2_installed()) return()

  experiments <- list(
    create_test_gen_experiment(),
    create_test_cand_experiment()
  )
  experiments[[1]]$inference_model$site_model <- create_gtr_site_model()
  testit::assert(
    experiments[[1]]$inference_model$mcmc$chain_length ==
    experiments[[2]]$inference_model$mcmc$chain_length
  )

  pir_params <- create_test_pir_params(
    twinning_params = create_twinning_params(),
    experiments = experiments,
    evidence_filename = get_temp_evidence_filename()
  )

  pir_out <- pir_run(
    phylogeny =  phylogeny <- ape::read.tree(text = "((A:2, B:2):1, C:3);"),
    pir_params = pir_params
  )
  expect_silent(check_pir_out(pir_out))
})


test_that("minimal use", {
  expect_silent(
    check_pir_out(
      pir_out = create_test_pir_run_output(
        add_best = TRUE,
        add_twin = TRUE
      )
    )
  )
})

test_that("abuse", {
  good_pir_out <- create_test_pir_run_output()
  expect_silent(check_pir_out(good_pir_out))

  # inference_model
  pir_out <- good_pir_out
  pir_out$inference_model <- "nonsense"
  pir_out$inference_model <- as.factor(pir_out$inference_model)
  expect_error(check_pir_out(pir_out), "inference_model")

  # site_model
  pir_out <- good_pir_out
  pir_out$site_model <- "nonsense"
  pir_out$site_model <- as.factor(pir_out$site_model)
  expect_error(check_pir_out(pir_out), "site_model")

  # clock_model
  pir_out <- good_pir_out
  pir_out$clock_model <- "nonsense"
  pir_out$clock_model <- as.factor(pir_out$clock_model)
  expect_error(check_pir_out(pir_out), "clock_model")

  # tree_prior
  pir_out <- good_pir_out
  pir_out$tree_prior <- "nonsense"
  pir_out$tree_prior <- as.factor(pir_out$tree_prior)
  expect_error(check_pir_out(pir_out), "tree_prior")
})

test_that("abuse by removing elements", {
  good_pir_out <- create_test_pir_run_output(
    add_best = TRUE,
    add_twin = TRUE
  )
  expect_silent(check_pir_out(good_pir_out))

  # Remove an element
  pir_out <- good_pir_out
  expect_silent(check_pir_out(pir_out))
  pir_out$tree <- NULL
  expect_error(check_pir_out(pir_out))

  # tree
  pir_out <- good_pir_out
  expect_silent(check_pir_out(pir_out))
  pir_out$tree <- "nonsense"
  expect_error(check_pir_out(pir_out))

  # inference_model
  pir_out <- good_pir_out
  expect_silent(check_pir_out(pir_out))
  pir_out$inference_model <- "nonsense"
  expect_error(check_pir_out(pir_out))

  # inference_model_weight
  pir_out <- good_pir_out
  expect_silent(check_pir_out(pir_out))
  pir_out$inference_model_weight <- -123.456
  expect_error(check_pir_out(pir_out))

  # inference_model_weight
  pir_out <- good_pir_out
  expect_silent(check_pir_out(pir_out))
  pir_out$inference_model_weight <- 123.456
  expect_error(check_pir_out(pir_out))

  # inference_model_weight
  pir_out <- good_pir_out
  expect_silent(check_pir_out(pir_out))
  pir_out$inference_model_weight <- "nonsense"
  expect_error(check_pir_out(pir_out))

  # site_model
  pir_out <- good_pir_out
  expect_silent(check_pir_out(pir_out))
  pir_out$site_model <- "nonsense"
  expect_error(check_pir_out(pir_out))

  # clock_model
  pir_out <- good_pir_out
  expect_silent(check_pir_out(pir_out))
  pir_out$clock_model <- "nonsense"
  expect_error(check_pir_out(pir_out))

  # tree_prior
  pir_out <- good_pir_out
  expect_silent(check_pir_out(pir_out))
  pir_out$tree_prior <- "nonsense"
  expect_error(check_pir_out(pir_out))

  # error_1
  pir_out <- good_pir_out
  expect_silent(check_pir_out(pir_out))
  pir_out$error_1 <- -123.456
  expect_error(check_pir_out(pir_out))

  # error_1
  pir_out <- good_pir_out
  expect_silent(check_pir_out(pir_out))
  pir_out$error_1[2] <- "nonsense"
  expect_error(check_pir_out(pir_out))
})
