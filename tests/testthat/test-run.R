context("run")

test_that("use", {

  phylogeny <- ape::read.tree(text = "(((A:1,B:1):1,C:2):1,D:3);")
  posterior <- run(phylogeny)
  testthat::expect_true(class(posterior) == "multiPhylo")

})
