context("test-create_twin_tree")

test_that("use", {
  tree <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  twin_tree <- create_twin_tree(tree)
  expect_equal(ape::Ntip(tree), ape::Ntip(twin_tree))

  # dist_tree <- ape::dist.nodes(tree)
  # dist_twin <- ape::dist.nodes(twin_tree)
  # order(dist_tree[1, ]) == order(dist_twin[1, ])
})

test_that("use", {
  tree <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  twin_tree <- create_twin_tree(tree)
  expect_equal(ape::Ntip(tree), ape::Ntip(twin_tree))
})
