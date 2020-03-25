context("test-create_generative_trees")

test_that("yule", {
  n_taxa <- 10
  crown_age <- 15
  tree <- create_yule_tree(n_taxa = n_taxa, crown_age = crown_age)
  expect_equal(ape::Ntip(tree), n_taxa)
  expect_equal(max(ape::branching.times(tree)), crown_age)
})

test_that("birth_death", {

  n_taxa <- 3
  crown_age <- 1
  tree <- create_bd_tree(n_taxa = n_taxa, crown_age = crown_age)
  expect_equal(ape::Ntip(tree), n_taxa)
  expect_equal(max(ape::branching.times(tree)), crown_age)
})

test_that("diversity_dependent, use", {
  n_taxa <- 10
  crown_age <- 15

  # This seed is selected for the shortness of 'create_exemplary_dd_tree'
  set.seed(15)
  tree <- create_exemplary_dd_tree(n_taxa = n_taxa, crown_age = crown_age)
  expect_equal(ape::Ntip(tree), n_taxa)
  expect_equal(max(ape::branching.times(tree)), crown_age)
})

test_that("diversity_dependent, use", {
  expect_error(create_exemplary_dd_tree(n_taxa = -123456, crown_age = 10))
  expect_error(create_exemplary_dd_tree(n_taxa = 10, crown_age = -123456))
  expect_error(
    create_exemplary_dd_tree(
      n_taxa = 10, crown_age = 10, extinction_rate = -123.456
    )
  )
  expect_error(
    create_exemplary_dd_tree(
      n_taxa = 10, crown_age = 10, best_of_n_trees = -123.456
    )
  )

  expect_silent(create_exemplary_dd_tree(n_taxa = 2, crown_age = 1))
  expect_silent(
    create_exemplary_dd_tree(n_taxa = 10, crown_age = 1, extinction_rate = 0.0)
  )
  expect_silent(
    create_exemplary_dd_tree(n_taxa = 10, crown_age = 1, best_of_n_trees = 1)
  )
})
