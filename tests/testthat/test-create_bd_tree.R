context("test-create_bd_tree")

test_that("use", {

  phylogeny <- load_tree(tree_model = "mbd", seed = 1)
  twinning_params <- create_twinning_params()
  twinning_params$rng_seed <- 1
  bd_sim <- create_bd_tree(
    phylogeny = phylogeny,
    twinning_params = twinning_params
  )
  bd_tree <- bd_sim$tree
  bd_l_matrix <- bd_sim$l_matrix

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
