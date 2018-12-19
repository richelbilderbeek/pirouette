context("test-create_twin_tree")

test_that("tree and twin tree have 3 taxa", {
  tree <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  twin_tree <- create_twin_tree(tree)
  expect_equal(ape::Ntip(tree), ape::Ntip(twin_tree))
})

test_that("node distances should remain in the same order, 3 taxa", {

  # Or:
  #  - taxa that are closest, should remain closest in the twin tree
  #  - taxa that are farthest, should remain farthest in the twin tree

  tree <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  twin_tree <- create_twin_tree(tree)
  n_tips <- ape::Ntip(tree)
  # Only care about nodes that are tips
  expect_equal(
    order(ape::dist.nodes(tree)[1:n_tips, 1:n_tips]),
    order(ape::dist.nodes(twin_tree)[1:n_tips, 1:n_tips])
  )
})

test_that("use", {
  tree <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  twin_tree <- create_twin_tree(tree)
  expect_equal(ape::Ntip(tree), ape::Ntip(twin_tree))
})

test_that("node distances should remain in the same order, 4 taxa, comb", {

  # Or:
  #  - taxa that are closest, should remain closest in the twin tree
  #  - taxa that are farthest, should remain farthest in the twin tree

  tree <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  twin_tree <- create_twin_tree(tree)
  n_tips <- ape::Ntip(tree)
  # Only care about nodes that are tips
  expect_equal(
    order(ape::dist.nodes(tree)[1:n_tips, 1:n_tips]),
    order(ape::dist.nodes(twin_tree)[1:n_tips, 1:n_tips])
  )
})

test_that("node distances should remain in the same order, 4 taxa", {

  skip("twin tree creation")
  # Or:
  #  - taxa that are closest, should remain closest in the twin tree
  #  - taxa that are farthest, should remain farthest in the twin tree

  # These work:
  #tree <- ape::read.tree(text = "((A:2, (B:1, C:1):1):1, D:3);")
  #tree <- ape::read.tree(text = "(B:3, ((D:1, C:1):1, A:2):1);")

  # This one fails.
  #
  # Note that the tree and twin tree do look similar in shape.
  # Qustion is: why does the test think something is wrong?
  tree <- ape::read.tree(text = "(t2:1.9827033,((t4:0.2338486712,t3:0.2338486712):0.4930762889,t1:0.7269249601):1.25577834);")
  ape::plot.phylo(tree)
  twin_tree <- create_twin_tree(tree)
  ape::plot.phylo(twin_tree)
  n_tips <- ape::Ntip(tree)
  # Only care about nodes that are tips
  expect_equal(
    order(ape::dist.nodes(tree)[1:n_tips, 1:n_tips]),
    order(ape::dist.nodes(twin_tree)[1:n_tips, 1:n_tips])
  )
})

test_that("node distances should remain in the same order, brute-force", {

  skip("twin tree creation")

  if (!is_on_travis()) return()
  # Or:
  #  - taxa that are closest, should remain closest in the twin tree
  #  - taxa that are farthest, should remain farthest in the twin tree
  for (i in seq(1, 100)) {
    set.seed(i)
    tree <- beastier:::create_random_phylogeny(n_taxa = 4)
    ape::write.tree(tree)
    ape::plot.phylo(tree)
    twin_tree <- create_twin_tree(tree)
    n_tips <- ape::Ntip(tree)
    # Only care about nodes that are tips
    expect_equal(
      order(ape::dist.nodes(tree)[1:n_tips, 1:n_tips]),
      order(ape::dist.nodes(twin_tree)[1:n_tips, 1:n_tips]),
      info = paste("seed:", i)
    )
  }
})
