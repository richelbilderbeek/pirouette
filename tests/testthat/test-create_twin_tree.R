context("test-create_twin_tree")

test_that("use", {
  tree <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  twin_tree <- create_twin_tree(tree)

  expect_equal(ape::Ntip(tree), ape::Ntip(twin_tree))
})
