test_that("use", {
  t <- get_tree_and_model_descriptions()
  expect_true("tree_and_model" %in% names(t))
  expect_true("description" %in% names(t))
  # This order is important: pir_plot expects this order
  expect_equal("true_generative", as.character(t$tree_and_model[1]))
  expect_equal("twin_generative", as.character(t$tree_and_model[2]))
  expect_equal("true_candidate", as.character(t$tree_and_model[3]))
  expect_equal("twin_candidate", as.character(t$tree_and_model[4]))
})
