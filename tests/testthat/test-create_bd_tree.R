context("create_bd_tree")

load_tree <- function(model = "mbd", seed = 1) {
  filename <- system.file(
    file.path(
      "extdata",
      "models",
      model
    ),
    paste0("tree_", seed),
    package = "pirouette"
  )
  if (!file.exists(filename)) {
    stop("This file does not exist! Try with different model name and/or seed.")
  }
  tree <- ape::read.tree(file = filename)
  tree
}

test_that("use", {

  mbd_tree <- load_tree(model = "mbd", seed = 1)

  bd_sim <- create_bd_tree(
    mbd_tree = mbd_tree,
    seed = 1
  )
  bd_tree <- bd_sim$bd_tree
  bd_l_matrix <- bd_sim$bd_l_matrix

  expect_equal(class(bd_tree), "phylo")
  expect_equal(
    max(ape::branching.times(bd_tree)),
    max(ape::branching.times(mbd_tree))
  )
  expect_equal(class(bd_l_matrix), "matrix")
  expect_equal(
    sum(bd_l_matrix[, 4] == -1),
    length(bd_tree$tip.label)
  )
})
