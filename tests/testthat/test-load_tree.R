context("test-load_tree")

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
