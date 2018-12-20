context("combine_brts_and_topology")

test_that("check usage with brts coming from the same tree", {

  skip("Rewrite to not depend on razzo")

  parameters <- razzo::open_parameters_file(
    razzo::get_path("parameters.csv")
  )
  parameters$seed <- 1
  tree <- razzo::create_mbd_tree(parameters)$mbd_tree
  brts <- convert_tree2brts(tree)

  max_seed <- 5
  for (seed in 1:max_seed) {
    parameters$seed <- seed
    tree <- razzo::create_mbd_tree(parameters)$mbd_tree
    brts <- convert_tree2brts(tree)

    test <- combine_brts_and_topology(
      brts = brts,
      tree = tree
    )

    testthat::expect_true(
      all(test$edge == tree$edge)
    )
    testthat::expect_true(
      all(test$Nnode == tree$Nnode)
    )
    testthat::expect_true(
      all(test$tip.label == tree$tip.label)
    )
    testthat::expect_true(
      all(test$root.edge == tree$root.edge)
    )
    testthat::expect_true(
      max(unname(test$edge.length) - tree$edge.length) <
        max(tree$edge.length * 1e-6)
    )
  }
})

test_that("all the tree features (but the branching times) are preserved", {

  set.seed(1)
  age <- 10
  sim <- mbd::mbd_sim(
    pars = c(0.3, 0.1, 2, 0.1),
    n_0 = 2,
    age = age,
    cond = 1
  )
  tree <- sim$reconstructed_tree

  brts <- sort(c(
    age,
    runif(
      n = (length(sim$brts) - 1),
      min = 0,
      max = age - 0.001)
  ),
  decreasing = TRUE)

  test <- combine_brts_and_topology(
    brts = brts,
    tree = tree
  )

  testthat::expect_true(
    all(test$edge == tree$edge)
  )
  testthat::expect_true(
    all(test$Nnode == tree$Nnode)
  )
  testthat::expect_true(
    all(test$tip.label == tree$tip.label)
  )
  testthat::expect_true(
    all(test$root.edge == tree$root.edge)
  )
})

test_that("abuse", {

  parameters <- razzo::open_parameters_file(
    razzo::get_path("parameters.csv")
  )
  tree <- razzo::create_mbd_tree(parameters)$mbd_tree
  brts0 <- convert_tree2brts(tree)
  brts <- brts0[1:floor(length(brts0) / 2)]

  testthat::expect_error(
    combine_brts_and_topology(
      brts = brts,
      tree = tree
    ),
    "brts must be same length as number of nodes on input tree"
  )
})
