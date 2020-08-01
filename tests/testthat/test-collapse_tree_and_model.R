test_that("only generative", {
  tree_and_model <- c(
    "true_generative"
  )
  tree_and_model <- as.factor(tree_and_model)
  inference_model <- collapse_tree_and_model(tree_and_model)

  expected <- c(
    "generative"
  )
  expect_equal(expected, as.character(inference_model))
})

test_that("twin", {
  tree_and_model <- c(
    "true_generative",
    "twin_generative"
  )
  tree_and_model <- as.factor(tree_and_model)
  inference_model <- collapse_tree_and_model(tree_and_model)

  expected <- c(
    "generative",
    "generative"
  )
  expect_equal(expected, as.character(inference_model))
})

test_that("candidate", {
  tree_and_model <- c(
    "true_candidate",
    "twin_candidate"
  )
  tree_and_model <- as.factor(tree_and_model)
  inference_model <- collapse_tree_and_model(tree_and_model)

  expected <- c(
    "candidate",
    "candidate"
  )
  expect_equal(expected, as.character(inference_model))
})

test_that("cand + twin", {
  tree_and_model <- c(
    "true_generative",
    "twin_generative",
    "true_candidate",
    "twin_candidate"
  )
  tree_and_model <- as.factor(tree_and_model)
  inference_model <- collapse_tree_and_model(tree_and_model)

  expected <- c(
    "generative",
    "generative",
    "candidate",
    "candidate"
  )
  expect_equal(expected, as.character(inference_model))
})

