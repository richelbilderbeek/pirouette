context("test-get_nltt_error_function")

test_that("use", {

  phylogeny <- ape::read.tree(text = "((A:1.5, B:1.5):1.5, C:3.0);")

  tree_1 <- ape::read.tree(text = "((A:1.0, B:1.0):2.0, C:3.0);")
  tree_2 <- ape::read.tree(text = "((A:2.0, B:2.0):1.0, C:3.0);")

  lowest_error <- get_nltt_error_function()(phylogeny, c(phylogeny))
  error_1 <- get_nltt_error_function()(phylogeny, c(tree_1))
  error_2 <- get_nltt_error_function()(phylogeny, c(tree_2))
  expect_true(lowest_error < error_1)
  expect_true(lowest_error < error_2)
})
