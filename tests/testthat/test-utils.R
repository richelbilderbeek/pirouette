context("test-utils")

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

test_that("get_twin_models", {
  expect_true(
    length(get_twin_models()) > 0 # nolint internal function
  )
  expect_true(
    is.character(get_twin_models()) # nolint internal function
  )
})

test_that("bd_phylo_2_l_table", {

  phylogeny <- load_tree(tree_model = "mbd", seed = 1)
  twinning_params <- create_twinning_params()
  twinning_params$rng_seed <- 1
  bd_sim <- create_bd_tree(
    phylogeny = phylogeny,
    twinning_params = twinning_params
  )
  bd_tree <- bd_sim$tree
  bd_l_matrix <- bd_sim$l_matrix

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

  # test brt <- brt + abs(min(brt))
  minus_bd_l_matrix <- bd_l_matrix
  minus_bd_l_matrix[, 1] <- minus_bd_l_matrix[, 1] - 10
  new_minus_bd_l_matrix <- bd_phylo_2_l_table(DDD::L2phylo(
    minus_bd_l_matrix,
    dropextinct = FALSE
  ))
  expect_true(
    all.equal(minus_bd_l_matrix, new_minus_bd_l_matrix)
  )
})
