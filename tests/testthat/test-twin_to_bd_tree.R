context("test-twin_to_bd_tree")

test_that("use", {

  phylogeny <- load_tree(tree_model = "mbd", seed = 1)
  twinning_params <- create_twinning_params()
  twinning_params$rng_seed_tree <- 1
  bd_tree <- twin_to_bd_tree(
    phylogeny = phylogeny,
    twinning_params = twinning_params
  )

  expect_equal(class(bd_tree), "phylo")
  expect_equal(
    max(ape::branching.times(bd_tree)),
    max(ape::branching.times(phylogeny))
  )
})
