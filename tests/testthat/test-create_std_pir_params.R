test_that("use", {
  if (rappdirs::app_dir()$os == "win") return()
  expect_silent(check_pir_params(create_std_pir_params()))
})