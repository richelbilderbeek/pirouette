context("test-check_pir_out")

test_that("simulated data", {
  if (!beastier::is_on_travis()) return()
  if (!beastier::is_beast2_installed()) return()

  pir_out <- pir_run(
    phylogeny =  phylogeny <- ape::read.tree(text = "((A:2, B:2):1, C:3);"),
    pir_params = create_test_pir_params(
      twinning_params = create_twinning_params(),
      experiments = list(
        create_test_gen_experiment(),
        create_test_cand_experiment()
      )
    )
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
