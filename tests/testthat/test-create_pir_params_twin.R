context("test-create_pir_params")

test_that("generative", {
  pir_params <- create_test_pir_params(
    twinning_params = create_twinning_params()
  )
  expect_silent(create_pir_params_twin(pir_params))
})

test_that("one candidate", {
  if (rappdirs::app_dir()$os == "win") return()

  pir_params <- create_test_pir_params(
    experiments = list(create_test_cand_experiment()),
    twinning_params = create_twinning_params()
  )
  expect_silent(create_pir_params_twin(pir_params))
})

test_that("two candidates", {
  if (rappdirs::app_dir()$os == "win") return()

  experiments <- list(
    create_test_cand_experiment(),
    create_test_cand_experiment()
  )
  experiments[[1]]$beast2_options <- experiments[[2]]$beast2_options
  experiments[[1]]$errors_filename <- experiments[[2]]$errors_filename
  # Experiments must have different inference models
  experiments[[1]]$inference_model$site_model <-
    beautier::create_tn93_site_model()
  experiments[[2]]$inference_model$site_model <-
    beautier::create_gtr_site_model()

  pir_params <- create_test_pir_params(
    experiments = experiments,
    twinning_params = create_twinning_params()
  )
  expect_silent(create_pir_params_twin(pir_params))
})

test_that("one generative, two candidates", {
  if (rappdirs::app_dir()$os == "win") return()

  experiments <- list(
    create_test_gen_experiment(),
    create_test_cand_experiment(),
    create_test_cand_experiment()
  )
  # Candidates must have different model than generative model
  experiments[[2]]$inference_model$site_model <-
    beautier::create_tn93_site_model()
  experiments[[3]]$inference_model$site_model <-
    beautier::create_gtr_site_model()
  # Candidates must share same BEAST2 options
  experiments[[2]]$beast2_options <- experiments[[3]]$beast2_options
  experiments[[2]]$errors_filename <- experiments[[3]]$errors_filename

  pir_params <- create_test_pir_params(
    experiments = experiments,
    twinning_params = create_twinning_params()
  )
  expect_silent(create_pir_params_twin(pir_params))
})

test_that("abuse", {
  expect_error(
    create_pir_params_twin(
      pir_params = create_test_pir_params()
    ),
    "'pir_params\\$twinning_params' must be a twinning parameter set"
  )
})
