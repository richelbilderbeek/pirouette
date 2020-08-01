test_that("only generative", {
  inference_model <- c(
    "generative"
  )
  inference_model <- as.factor(inference_model)
  expect_silent(relevel_inference_model(inference_model))
})

test_that("only candidates", {
  # Misordered on purpose
  inference_model <- c(
    "candidate",
    "candidate"
  )
  inference_model <- as.factor(inference_model)
  expect_silent(relevel_inference_model(inference_model))
})

test_that("both", {
  # Misordered on purpose
  inference_model <- c(
    "candidate",
    "candidate",
    "generative",
    "generative"
  )
  inference_model <- as.factor(inference_model)
  expected_levels <- c(
    "generative",
    "candidate"
  )
  expect_true(!all(levels(inference_model) == expected_levels))

  inference_model <- relevel_inference_model(inference_model)

  expect_true(all(levels(inference_model) == expected_levels))
})
