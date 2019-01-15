context("test-get_crown_age")

test_that("use", {
  phylogeny <- ape::read.tree(text = "((A:1, B:1):1, (C:1, D:1):1);")
  crown_age <- get_crown_age(phylogeny)
  testit::assert(crown_age == 2.0)
})

test_that("get_crown_age: abuse", {
  expect_error(
    get_crown_age(phylogeny = c(1, 2, 3)),
    "'phylogeny' must be of class 'phylo'"
  )
})
