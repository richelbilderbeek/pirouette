context("test-create_twin_branching_times")

test_that("use", {

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")

  branching_times <- create_twin_branching_times(
    seed = 314,
    lambda = 0.1,
    mu = 1.0,
    phylogeny = phylogeny,
    n_replicates = 1,
    method = "random_tree"
  )
  expect_equal(length(ape::branching.times(phylogeny)), length(branching_times))
})

test_that("all methods must be OK", {

  methods <- c("random_tree", "max_clade_cred", "max_likelihood")
  for (method in methods) {
    expect_silent(
      create_twin_branching_times(
        seed = 314,
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

  expect_silent(
    create_twin_branching_times(
      seed = 314,
      lambda = 0.1,
      mu = 1.0,
      phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
      n_replicates = 1,
      method = "random_tree"
    )
  )

  expect_error(
    create_twin_branching_times(
      seed = "nonsense",
      lambda = 0.1,
      mu = 1.0,
      phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
      n_replicates = 1,
      method = "random_tree"
    ),
    "'seed' must be numerical"
  )

  expect_error(
    create_twin_branching_times(
      seed = c(1, 2), # nonsense
      lambda = 0.1,
      mu = 1.0,
      phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
      n_replicates = 1,
      method = "random_tree"
    ),
    "'seed' must be one numerical value"
  )

  expect_error(
    create_twin_branching_times(
      seed = 314,
      lambda = "nonsense",
      mu = 1.0,
      phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
      n_replicates = 1,
      method = "random_tree"
    ),
    "'lambda' must be numerical"
  )

  expect_error(
    create_twin_branching_times(
      seed = 314,
      lambda = c(1, 2),
      mu = 1.0,
      phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
      n_replicates = 1,
      method = "random_tree"
    ),
    "'lambda' must be one numerical value"
  )

  expect_error(
    create_twin_branching_times(
      seed = 314,
      lambda = 0.1,
      mu = "nonsense",
      phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
      n_replicates = 1,
      method = "random_tree"
    ),
    "'mu' must be numerical"
  )

  expect_error(
    create_twin_branching_times(
      seed = 314,
      lambda = 0.1,
      mu = c(1, 2),
      phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
      n_replicates = 1,
      method = "random_tree"
    ),
    "'mu' must be one numerical value"
  )

  expect_error(
    create_twin_branching_times(
      seed = 314,
      lambda = 0.1,
      mu = 1.0,
      phylogeny = "nonsense",
      n_replicates = 1,
      method = "random_tree"
    ),
    "phylogeny' must be a valid phylogeny"
  )

  expect_error(
    create_twin_branching_times(
      seed = 314,
      lambda = 0.1,
      mu = 1.0,
      phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
      n_replicates = "nonsense",
      method = "random_tree"
    ),
    "'n_replicates' must be numerical"
  )

    expect_error(
    create_twin_branching_times(
      seed = 314,
      lambda = 0.1,
      mu = 1.0,
      phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
      n_replicates = c(1, 2), # nonsense,
      method = "random_tree"
    ),
    "'n_replicates' must be one numerical value"
  )

  expect_error(
    create_twin_branching_times(
      seed = 314,
      lambda = 0.1,
      mu = 1.0,
      phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
      n_replicates = 1,
      method = "nonsense"
    ),
    "'method' not in the supported methods"
  )

})
