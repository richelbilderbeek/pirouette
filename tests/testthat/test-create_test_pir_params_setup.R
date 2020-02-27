test_that("minimal use", {
  expect_silent(check_pir_params(create_test_pir_params_setup()))
})

test_that("gen", {
  pir_params <- create_test_pir_params_setup(
    has_candidate = FALSE,
    has_twinning = FALSE
  )
  expect_true(!has_twinning(pir_params))
  expect_true(!has_candidate_experiments(pir_params))
})

test_that("gen + cand", {
  if (rappdirs::app_dir()$os == "win") return()

  pir_params <- create_test_pir_params_setup(
    has_candidate = TRUE,
    has_twinning = FALSE
  )
  expect_true(!has_twinning(pir_params))
  expect_true(has_candidate_experiments(pir_params))
})

test_that("gen + twin", {
  pir_params <- create_test_pir_params_setup(
    has_candidate = FALSE,
    has_twinning = TRUE
  )
  expect_true(has_twinning(pir_params))
  expect_true(!has_candidate_experiments(pir_params))
})

test_that("gen + cand + twin", {
  if (rappdirs::app_dir()$os == "win") return()

  pir_params <- create_test_pir_params_setup(
    has_candidate = TRUE,
    has_twinning = TRUE
  )
  expect_true(has_twinning(pir_params))
  expect_true(has_candidate_experiments(pir_params))
})
