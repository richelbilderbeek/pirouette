context("test-create_stunning_tree")

test_that("default use must be silent", {

  expect_silent(create_stunning_tree())
})

test_that("number of taxa and tips must match", {

  skip("Issue 129, #129")
  n_taxa <- 3
  phylo <- create_stunning_tree(n_taxa = n_taxa)
})
