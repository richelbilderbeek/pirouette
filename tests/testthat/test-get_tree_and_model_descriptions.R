test_that("use", {
  t <- get_tree_and_model_descriptions()
  expect_true("tree_and_model" %in% names(t))
  expect_true("description" %in% names(t))
})
