context("test-create_bd_tree")

test_that("use", {

  phylogeny <- load_tree(tree_model = "mbd", seed = 1)

  yule_sim <- create_yule_tree(
    phylogeny = phylogeny,
    seed = 1
  )
  yule_tree <- yule_sim$tree
  yule_l_matrix <- yule_sim$l_matrix

  expect_equal(class(yule_tree), "phylo")
  expect_equal(
    max(ape::branching.times(yule_tree)),
    max(ape::branching.times(phylogeny))
  )
  expect_equal(class(yule_l_matrix), "matrix")
  expect_equal(
    sum(yule_l_matrix[, 4] == -1),
    length(yule_tree$tip.label)
  )
})
