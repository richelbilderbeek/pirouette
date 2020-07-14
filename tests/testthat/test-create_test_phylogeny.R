test_that("use", {
  phylogeny <- create_test_phylogeny()
  expect_equal(3, ape::Ntip(phylogeny))
})
