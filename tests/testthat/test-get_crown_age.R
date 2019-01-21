context("test-get_crown_age")

test_that("use", {
  expect_equal(
    2.0, get_crown_age(ape::read.tree(text = "((A:1, B:1):1, (C:1, D:1):1);"))
  )
  expect_equal(
    2.0, get_crown_age(ape::read.tree(text = "((A:1, B:1):1, C:2);"))
  )
  expect_equal(
    3.0, get_crown_age(ape::read.tree(text = "((A:2, B:2):1, C:3);"))
  )
  expect_equal(
    4.0, get_crown_age(ape::read.tree(text = "((A:2, B:2):2, C:4);"))
  )
})

test_that("get_crown_age: abuse", {
  expect_error(
    get_crown_age(phylogeny = c(1, 2, 3)),
    "'phylogeny' must be of class 'phylo'"
  )
})
