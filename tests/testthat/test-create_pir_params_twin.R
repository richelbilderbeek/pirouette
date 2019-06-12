test_that("generative", {
  pir_params <- create_test_pir_params(
    twinning_params = create_twinning_params()
  )
  expect_silent(create_pir_params_twin(pir_params))
})

test_that("one candidate", {
  pir_params <- create_test_pir_params(
    experiments = list(create_test_cand_experiment()),
    twinning_params = create_twinning_params()
  )
  expect_silent(create_pir_params_twin(pir_params))
})

test_that("two candidates", {
  experiments <- list(
    create_test_cand_experiment(),
    create_test_cand_experiment()
  )
  pir_params <- create_test_pir_params(
    experiments = experiments,
    twinning_params = create_twinning_params()
  )
  expect_silent(create_pir_params_twin(pir_params))
})

test_that("one generative, two candidates", {
  experiments <- list(
    create_test_gen_experiment(),
    create_test_cand_experiment(),
    create_test_cand_experiment()
  )
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
