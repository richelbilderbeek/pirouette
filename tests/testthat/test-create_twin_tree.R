context("test-create_twin_tree")

dist_nodes <- function(tree, precision = 12) {
  DDD::roundn(
    ape::dist.nodes(tree),
    digits = precision
  )
}

test_that("tree and twin tree have 3 taxa", {
  tree <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  for (twin_model in get_twin_models()) {
    twin_tree <- create_twin_tree(
      phylogeny = tree,
      twin_model = twin_model
    )
    expect_equal(ape::Ntip(tree), ape::Ntip(twin_tree))
  }
})

test_that("node distances should remain in the same order, 3 taxa", {

  # Or:
  #  - taxa that are closest, should remain closest in the twin tree
  #  - taxa that are farthest, should remain farthest in the twin tree

  tree <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  for (twin_model in get_twin_models()) {
    twin_tree <- create_twin_tree(
      phylogeny = tree,
      twin_model = twin_model
    )
    n_tips <- ape::Ntip(tree)
    # Only care about nodes that are tips
    expect_equal(
      order(dist_nodes(tree)[1:n_tips, 1:n_tips]),
      order(dist_nodes(twin_tree)[1:n_tips, 1:n_tips])
    )
  }
})

test_that("use", {

  tree <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  for (twin_model in get_twin_models()) {
    twin_tree <- create_twin_tree(
      phylogeny = tree,
      twin_model = twin_model
    )
    expect_equal(ape::Ntip(tree), ape::Ntip(twin_tree))
  }
})

test_that("node distances should remain in the same order, 4 taxa, easy", {

  # Or:
  #  - taxa that are closest, should remain closest in the twin tree
  #  - taxa that are farthest, should remain farthest in the twin tree

  tree <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  for (twin_model in get_twin_models()) {
    twin_tree <- create_twin_tree(
      phylogeny = tree,
      twin_model = twin_model
    )
    n_tips <- ape::Ntip(tree)
    # Only care about node distances between tips
    expect_equal(
      order(dist_nodes(tree)[1:n_tips, 1:n_tips]),
      order(dist_nodes(twin_tree)[1:n_tips, 1:n_tips])
    )
  }
})

test_that("node distances should remain in the same order, 4 taxa, hard", {

  tree <- ape::read.tree(text = "((A:2, (B:1, C:1):1):1, D:3);")
  for (twin_model in get_twin_models()) {
    twin_tree <- create_twin_tree(
      phylogeny = tree,
      twin_model = twin_model
    )
    n_tips <- ape::Ntip(tree)
    # Only care about node distances between tips
    expect_equal(
      order(dist_nodes(tree)[1:n_tips, 1:n_tips]),
      order(dist_nodes(twin_tree)[1:n_tips, 1:n_tips])
    )
  }
})

test_that("node distances should remain in the same order, 4 taxa, harder", {

  tree <- ape::read.tree(text = "(B:3, ((D:1, C:1):1, A:2):1);")
  for (twin_model in get_twin_models()) {
    twin_tree <- create_twin_tree(
      phylogeny = tree,
      twin_model = twin_model
    )
    n_tips <- ape::Ntip(tree)
    # Only care about node distances between tips
    expect_equal(
      order(dist_nodes(tree)[1:n_tips, 1:n_tips]),
      order(dist_nodes(twin_tree)[1:n_tips, 1:n_tips])
    )
  }
})

test_that("node distances should remain in the same order, 4 taxa", {

  # Or:
  #  - taxa that are closest, should remain closest in the twin tree
  #  - taxa that are farthest, should remain farthest in the twin tree

  # These work:
  #
  # tree <- ape::read.tree(text = "((A:2, (B:1, C:1):1):1, D:3);") # nolint put commented code here as example
  # tree <- ape::read.tree(text = "(B:3, ((D:1, C:1):1, A:2):1);") # nolint put commented code here as example
  #
  # This one fails.
  #
  # Note that the tree and twin tree do look similar in shape.
  # Question is: why does the test think something is wrong?
  tree <- ape::read.tree(text = "(t2:1.9827033,((t4:0.2338486712,t3:0.2338486712):0.4930762889,t1:0.7269249601):1.25577834);") # nolint indeed this is a long line, but it is what the brute-force below generated
  # if you want to plot: ape::plot.phylo(tree)
  for (twin_model in get_twin_models()) {
    twin_tree <- create_twin_tree(
      phylogeny = tree,
      twin_model = twin_model
    )
    # if you want to plot: ape::plot.phylo(twin_tree)
    n_tips <- ape::Ntip(tree)
    # Only care about node distances between tips
    expect_equal(
      order(dist_nodes(tree)[1:n_tips, 1:n_tips]),
      order(dist_nodes(twin_tree)[1:n_tips, 1:n_tips])
    )
  }
})

test_that("node distances should remain in the same order, brute-force", {

  # Or:
  #  - taxa that are closest, should remain closest in the twin tree
  #  - taxa that are farthest, should remain farthest in the twin tree
  max_i <- 5 + beastier::is_on_travis() * 95
  for (i in seq(1, max_i)) {
    set.seed(i)
    tree <- beastier:::create_random_phylogeny(n_taxa = 4)
    for (twin_model in get_twin_models()) {
      # if you want to plot: ape::write.tree(tree); ape::plot.phylo(tree)
      twin_tree <- create_twin_tree(
        phylogeny = tree,
        twin_model = twin_model
      )
      n_tips <- ape::Ntip(tree)
      # Only care about nodes that are tips
      expect_equal(
        order(dist_nodes(tree)[1:n_tips, 1:n_tips]),
        order(dist_nodes(twin_tree)[1:n_tips, 1:n_tips]),
        info = paste("seed:", i)
      )
    }
  }
})

test_that("abuse", {
  tree <- ape::read.tree(text = "(B:3, ((D:1, C:1):1, A:2):1);")
  expect_error(
    create_twin_tree(
      phylogeny = tree,
      twin_model = "nonsense"
    )
  )
})
