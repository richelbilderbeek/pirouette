test_that("only generative", {
  tree_and_model <- c(
    "true_generative"
  )
  tree_and_model <- as.factor(tree_and_model)
  expect_silent(relevel_tree_and_model(tree_and_model))
})

test_that("only candidates", {
  # Misordered on purpose
  tree_and_model <- c(
    "twin_candidate",
    "true_candidate"
  )
  tree_and_model <- as.factor(tree_and_model)
  expected_levels <- c(
    "true_candidate",
    "twin_candidate"
  )
  # Already works, because the levels are put on alphabet
  expect_true(all(levels(tree_and_model) == expected_levels))

  tree_and_model <- relevel_tree_and_model(tree_and_model)

  expect_true(all(levels(tree_and_model) == expected_levels))
})

test_that("only twins", {
  # Misordered on purpose
  tree_and_model <- c(
    "twin_candidate",
    "twin_generative"
  )
  tree_and_model <- as.factor(tree_and_model)
  expected_levels <- c(
    "twin_generative",
    "twin_candidate"
  )
  expect_true(!all(levels(tree_and_model) == expected_levels))

  tree_and_model <- relevel_tree_and_model(tree_and_model)

  expect_true(all(levels(tree_and_model) == expected_levels))
})

test_that("all four levels", {
  # Misordered on purpose
  tree_and_model <- c(
    "twin_candidate",
    "true_candidate",
    "twin_generative",
    "true_generative"
  )
  tree_and_model <- as.factor(tree_and_model)
  expected_levels <- c(
    "true_generative",
    "twin_generative",
    "true_candidate",
    "twin_candidate"
  )
  expect_true(!all(levels(tree_and_model) == expected_levels))

  tree_and_model <- relevel_tree_and_model(tree_and_model)

  expect_true(all(levels(tree_and_model) == expected_levels))
})
