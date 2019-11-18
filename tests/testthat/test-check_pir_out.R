context("test-check_pir_out")

test_that("simulated data", {
  if (!beastier::is_on_travis()) return()
  if (!beastier::is_beast2_installed()) return()

  experiments <- list(
    pirouette::create_test_gen_experiment(),
    pirouette::create_test_cand_experiment()
  )
  experiments[[1]]$inference_model$site_model <-
    beautier::create_gtr_site_model()
  testit::assert(
    experiments[[1]]$inference_model$mcmc$chain_length ==
    experiments[[2]]$inference_model$mcmc$chain_length
  )

  pirouette::check_experiments(experiments)

  pir_out <- pirouette::pir_run(
    phylogeny =  phylogeny <- ape::read.tree(text = "((A:2, B:2):1, C:3);"),
    pir_params = pirouette::create_test_pir_params(
      twinning_params = pirouette::create_twinning_params(),
      experiments = experiments
    )
  )
  testthat::expect_silent(pirouette::check_pir_out(pir_out))
})


test_that("minimal use", {
  testthat::expect_silent(
    pirouette::check_pir_out(
      pir_out = pirouette::create_test_pir_run_output(
        add_best = TRUE,
        add_twin = TRUE
      )
    )
  )
})

test_that("abuse", {
  good_pir_out <- pirouette::create_test_pir_run_output(
    add_best = TRUE,
    add_twin = TRUE
  )
  testthat::expect_silent(pirouette::check_pir_out(good_pir_out))

  # Remove an element
  pir_out <- good_pir_out
  testthat::expect_silent(pirouette::check_pir_out(pir_out))
  pir_out$tree <- NULL
  testthat::expect_error(pirouette::check_pir_out(pir_out))

  # tree
  pir_out <- good_pir_out
  testthat::expect_silent(pirouette::check_pir_out(pir_out))
  pir_out$tree <- "nonsense"
  testthat::expect_error(pirouette::check_pir_out(pir_out))

  # inference_model
  pir_out <- good_pir_out
  testthat::expect_silent(pirouette::check_pir_out(pir_out))
  pir_out$inference_model <- "nonsense"
  testthat::expect_error(pirouette::check_pir_out(pir_out))

  # inference_model_weight
  pir_out <- good_pir_out
  testthat::expect_silent(pirouette::check_pir_out(pir_out))
  pir_out$inference_model_weight <- -123.456
  testthat::expect_error(pirouette::check_pir_out(pir_out))

  # inference_model_weight
  pir_out <- good_pir_out
  testthat::expect_silent(pirouette::check_pir_out(pir_out))
  pir_out$inference_model_weight <- 123.456
  testthat::expect_error(pirouette::check_pir_out(pir_out))

  # inference_model_weight
  pir_out <- good_pir_out
  testthat::expect_silent(pirouette::check_pir_out(pir_out))
  pir_out$inference_model_weight <- "nonsense"
  testthat::expect_error(pirouette::check_pir_out(pir_out))

  # site_model
  pir_out <- good_pir_out
  testthat::expect_silent(pirouette::check_pir_out(pir_out))
  pir_out$site_model <- "nonsense"
  testthat::expect_error(pirouette::check_pir_out(pir_out))

  # clock_model
  pir_out <- good_pir_out
  testthat::expect_silent(pirouette::check_pir_out(pir_out))
  pir_out$clock_model <- "nonsense"
  testthat::expect_error(pirouette::check_pir_out(pir_out))

  # tree_prior
  pir_out <- good_pir_out
  testthat::expect_silent(pirouette::check_pir_out(pir_out))
  pir_out$tree_prior <- "nonsense"
  testthat::expect_error(pirouette::check_pir_out(pir_out))

  # error_1
  pir_out <- good_pir_out
  testthat::expect_silent(pirouette::check_pir_out(pir_out))
  pir_out$error_1 <- -123.456
  testthat::expect_error(pirouette::check_pir_out(pir_out))

  # error_1
  pir_out <- good_pir_out
  testthat::expect_silent(pirouette::check_pir_out(pir_out))
  pir_out$error_1[2] <- "nonsense"
  testthat::expect_error(pirouette::check_pir_out(pir_out))
})
