context("test-create_generative_trees")

test_that("yule", {
  n_taxa <- 10
  crown_age <- 15
  tree <- create_yule_tree(n_taxa = n_taxa, crown_age = crown_age)
  expect_equal(ape::Ntip(tree), n_taxa)
  expect_equal(max(ape::branching.times(tree)), crown_age)
})

test_that("bd", {
  n_taxa <- 10
  crown_age <- 15
  tree <- create_bd_tree(n_taxa = n_taxa, crown_age = crown_age)
  expect_equal(ape::Ntip(tree), n_taxa)
  expect_equal(max(ape::branching.times(tree)), crown_age)
})
