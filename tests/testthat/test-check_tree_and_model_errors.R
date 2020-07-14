test_that("use", {
  tree_and_model_errors <- tibble::tibble(
    tree_and_model = get_tree_and_model_values(),
    error_value = runif(n = 4)
  )
  tree_and_model_errors$tree_and_model <- forcats::as_factor(
    tree_and_model_errors$tree_and_model
  )

  expect_silent(check_tree_and_model_errors(tree_and_model_errors))
})

test_that("abuse", {
  good <- tibble::tibble(
    tree_and_model = get_tree_and_model_values(),
    error_value = runif(n = 4)
  )
  good$tree_and_model <- forcats::as_factor(
    good$tree_and_model
  )
  expect_silent(check_tree_and_model_errors(good))

  # Remove factor
  bad <- good
  bad$tree_and_model <- as.character(bad$tree_and_model)
  expect_error(
    check_tree_and_model_errors(bad),
    "'tree_and_model_errors.tree_and_model' must be a factor"
  )

  # Remove column
  bad <- good
  bad$error_value <- NULL
  expect_error(
    check_tree_and_model_errors(bad),
    "'tree_and_model_errors' must have 2 columns"
  )

  # Add column
  bad <- good
  bad$nonsense <- "nonsense"
  expect_error(
    check_tree_and_model_errors(bad),
    "'tree_and_model_errors' must have 2 columns"

  )

  # Rename 'tree_and_model' column
  bad <- good %>% dplyr::rename("nonsense" = tree_and_model)
  expect_error(
    check_tree_and_model_errors(bad),
    "'tree_and_model_errors' must have a column named 'tree_and_model'"
  )

  # Rename 'error_value' column
  bad <- good %>% dplyr::rename("nonsense" = error_value)
  expect_error(
    check_tree_and_model_errors(bad),
    "'tree_and_model_errors' must have a column named 'error_value'"
  )
})
