context("test-create_test_pir_run_output")

test_that("use_no_twin_no_best", {
  errors <- create_test_pir_run_output(add_twin = FALSE, add_best = FALSE)
  check_pir_out(errors)
  expect_true("true" %in% errors$tree)
  expect_false("twin" %in% errors$tree)
  expect_true("generative" %in% errors$inference_model)
  expect_false("candidate" %in% errors$inference_model)
})

test_that("use_twin_best", {
  errors <- create_test_pir_run_output(add_twin = TRUE, add_best = TRUE)
  check_pir_out(errors)
  expect_true("true" %in% errors$tree)
  expect_true("twin" %in% errors$tree)
  expect_true("generative" %in% errors$inference_model)
  expect_true("candidate" %in% errors$inference_model)
})
