context("test-twin_to_yule_tree")

test_that("use", {

  phylogeny <- load_tree(tree_model = "mbd", seed = 1)
  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  twinning_params <- create_twinning_params()
  twinning_params$rng_seed_twin_tree <- 1
  yule_tree <- twin_to_yule_tree(
    phylogeny = phylogeny,
    twinning_params = twinning_params
  )

  expect_equal(class(yule_tree), "phylo")
  expect_equal(
    max(ape::branching.times(yule_tree)),
    max(ape::branching.times(phylogeny))
  )
})
