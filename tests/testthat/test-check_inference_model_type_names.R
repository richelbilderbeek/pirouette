test_that("use", {
  expect_silent(check_inference_model_type_names("generative"))
  expect_silent(check_inference_model_type_names("candidate"))
  expect_silent(check_inference_model_type_names(c("candidate", "generative")))
  expect_error(check_inference_model_type_names("nonsense"))
  expect_error(check_minference_odel_type_names(NA))
})
