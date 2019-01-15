context("create_bd_tree")

<<<<<<< HEAD
test_that("use", {

  phylogeny <- load_tree(model = "mbd", seed = 1)

  bd_sim <- create_bd_tree(
    phylogeny = phylogeny,
=======
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
>>>>>>> a42cf5595f931aa81cbd1d0b58618ef63b2e8746
    seed = 1
  )
  bd_tree <- bd_sim$bd_tree
  bd_l_matrix <- bd_sim$bd_l_matrix

  expect_equal(class(bd_tree), "phylo")
  expect_equal(
    max(ape::branching.times(bd_tree)),
<<<<<<< HEAD
    max(ape::branching.times(phylogeny))
=======
    max(ape::branching.times(mbd_tree))
>>>>>>> a42cf5595f931aa81cbd1d0b58618ef63b2e8746
  )
  expect_equal(class(bd_l_matrix), "matrix")
  expect_equal(
    sum(bd_l_matrix[, 4] == -1),
    length(bd_tree$tip.label)
  )
})
