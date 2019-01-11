context("test-phylo_to_nltts")

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

  mcmc <- create_mcmc(chain_length = 2000)
  n_base_pairs <- 4
  seed <- 1
  phylogeny <- load_tree(model = "mbd", seed = seed)
  nltts <- phylo_to_nltts(
    phylogeny = phylogeny,
    mcmc = mcmc,
    n_base_pairs = n_base_pairs,
    seed = seed
  )
  expect_true(
    length(nltts) > 0
  )
  expect_true(
    all(nltts > 0) & all(nltts < 1)
  )
})

test_that("abuse", {

  mcmc <- create_mcmc(chain_length = 2000)
  n_base_pairs <- 4
  seed <- 1
  phylogeny <- load_tree(model = "mbd", seed = seed)

  expect_error(
    phylo_to_nltts(
      phylogeny = c(3, 2, 1),
      mcmc = mcmc,
      n_base_pairs = n_base_pairs,
      seed = seed
    ),
    "parameter 'phylogeny' must be a phylogeny"
  )
  expect_error(
    phylo_to_nltts(
      phylogeny = load_tree(model = "mbd", seed = seed),
      mcmc = mcmc,
      n_base_pairs = n_base_pairs,
      seed = "nonsense"
    ),
    "'seed' must be a number"
  )
})
