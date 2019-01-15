context("utils")

test_that("get_site_models", {
  expect_true(
    length(get_site_models()) > 0 # nolint internal function
  )
  expect_true(
    is.character(get_site_models()) # nolint internal function
  )
})

test_that("get_clock_models", {
  expect_true(
    length(get_clock_models()) > 0 # nolint internal function
  )
  expect_true(
    is.character(get_clock_models()) # nolint internal function
  )
})

test_that("bd_phylo_2_l_table", {

  phylogeny <- load_tree(model = "mbd", seed = 1)
  bd_sim <- create_bd_tree(
    phylogeny = phylogeny,
    seed = 1
  )
  bd_tree <- bd_sim$bd_tree
  bd_l_matrix <- bd_sim$bd_l_matrix

  # test phylo -> L -> phylo
  bd_test_tree <- DDD::L2phylo(
    bd_phylo_2_l_table(bd_tree),
    dropextinct = FALSE
  )
  expect_equal(
    bd_test_tree$edge,
    bd_tree$edge
  )
  expect_equal(
    bd_test_tree$edge.length,
    unname(bd_tree$edge.length)
  )
  expect_equal(
    bd_test_tree$Nnode,
    bd_tree$Nnode
  )
  expect_equal(
    bd_test_tree$root.edge,
    bd_tree$root.edge
  )

  # test L -> phylo -> L
  test_bd_l_matrix <- bd_phylo_2_l_table(DDD::L2phylo(
    bd_l_matrix,
    dropextinct = FALSE
  ))
  expect_equal(
    test_bd_l_matrix,
    bd_l_matrix
  )
  created_col_names <- colnames(test_bd_l_matrix)
  expected_col_names <- c("birth_time", "parent", "id", "death_time")
  expect_equal(created_col_names, expected_col_names)
})
