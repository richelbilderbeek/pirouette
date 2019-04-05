context("test-load_tree")

test_that("use", {

  phylogeny <- load_tree()
  expect_true(is_phylo(phylogeny))
})

test_that("abuse", {
  expect_error(
    load_tree(
      tree_model = "mbd",
      seed = -40000
    )
  )
  expect_error(
    load_tree(
      tree_model = "pippobaudo",
      seed = 1
    )
  )
})
