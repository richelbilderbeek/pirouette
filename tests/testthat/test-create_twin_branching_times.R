context("test-create_twin_branching_times")

test_that("use", {

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")

  set.seed(314)
  branching_times <- pirouette::create_twin_branching_times(
    lambda = 0.1,
    mu = 1.0,
    phylogeny = phylogeny,
    n_replicates = 1,
    method = "random_tree"
  )
  testthat::expect_equal(
    length(ape::branching.times(phylogeny)), length(branching_times)
  )

})

test_that("all methods must be OK", {

  methods <- c("random_tree", "max_clade_cred", "max_likelihood")
  for (method in methods) {
    set.seed(314)
    testthat::expect_silent(
      pirouette::create_twin_branching_times(
        lambda = 0.1,
        mu = 1.0,
        phylogeny = ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);"),
        n_replicates = 1,
        method = method
      )
    )
  }

})

test_that("abuse", {

  set.seed(314)
  testthat::expect_silent(
    pirouette::create_twin_branching_times(
      lambda = 0.1,
      mu = 1.0,
      phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
      n_replicates = 1,
      method = "random_tree"
    )
  )

  set.seed(314)
  testthat::expect_error(
    pirouette::create_twin_branching_times(
      lambda = "nonsense",
      mu = 1.0,
      phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
      n_replicates = 1,
      method = "random_tree"
    ),
    "'lambda' must be one numerical value"
  )

  set.seed(314)
  testthat::expect_error(
    pirouette::create_twin_branching_times(
      lambda = c(1, 2),
      mu = 1.0,
      phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
      n_replicates = 1,
      method = "random_tree"
    ),
    "'lambda' must be one numerical value"
  )

  set.seed(314)
  testthat::expect_error(
    pirouette::create_twin_branching_times(
      lambda = 0.1,
      mu = "nonsense",
      phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
      n_replicates = 1,
      method = "random_tree"
    ),
    "'mu' must be one numerical value"
  )

  set.seed(314)
  testthat::expect_error(
    pirouette::create_twin_branching_times(
      lambda = 0.1,
      mu = c(1, 2),
      phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
      n_replicates = 1,
      method = "random_tree"
    ),
    "'mu' must be one numerical value"
  )

  set.seed(314)
  testthat::expect_error(
    pirouette::create_twin_branching_times(
      lambda = 0.1,
      mu = 1.0,
      phylogeny = "nonsense",
      n_replicates = 1,
      method = "random_tree"
    ),
    "phylogeny' must be a valid phylogeny"
  )

  set.seed(314)
  testthat::expect_error(
    pirouette::create_twin_branching_times(
      lambda = 0.1,
      mu = 1.0,
      phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
      n_replicates = "nonsense",
      method = "random_tree"
    ),
    "'n_replicates' must be one numerical value"
  )

  set.seed(314)
  testthat::expect_error(
    pirouette::create_twin_branching_times(
      lambda = 0.1,
      mu = 1.0,
      phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
      n_replicates = c(1, 2), # nonsense,
      method = "random_tree"
    ),
    "'n_replicates' must be one numerical value"
  )

  set.seed(314)
  testthat::expect_error(
    pirouette::create_twin_branching_times(
      lambda = 0.1,
      mu = 1.0,
      phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
      n_replicates = 1,
      method = "nonsense"
    ),
    "'method' not in the supported methods"
  )

})
