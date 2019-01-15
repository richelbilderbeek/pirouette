context("create_bd_tree")

test_that("use", {

  phylogeny <- load_tree(model = "mbd", seed = 1)

  bd_sim <- create_bd_tree(
    phylogeny = phylogeny,
    seed = 1
  )
  bd_tree <- bd_sim$bd_tree
  bd_l_matrix <- bd_sim$bd_l_matrix

  expect_equal(class(bd_tree), "phylo")
  expect_equal(
    max(ape::branching.times(bd_tree)),
    max(ape::branching.times(phylogeny))
  )
  expect_equal(class(bd_l_matrix), "matrix")
  expect_equal(
    sum(bd_l_matrix[, 4] == -1),
    length(bd_tree$tip.label)
  )
})
