test_that("use", {
  pir_params <- create_std_pir_params(os = "win")
  expect_equal(length(pir_params$experiments), 1)
  expect_silent(check_pir_params(pir_params))
})

test_that("use", {
  if (rappdirs::app_dir()$os == "win") return()
  expect_silent(check_pir_params(create_std_pir_params()))
})
