context("create_nltts")

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

  seed <- 1
  phylogeny <- load_tree(model = "mbd", seed = seed)
  n_base_pairs <- 4
  mcmc <- create_mcmc(chain_length = 2000)

  posterior <- phylo_to_posterior(
    phylogeny = phylogeny,
    alignment_params = create_alignment_params(
      root_sequence = create_blocked_dna(length = n_base_pairs),
      mutation_rate = 0.1,
      rng_seed = seed
    ),
    inference_params = create_inference_params(
      mcmc = mcmc,
      rng_seed = seed
    )
  )

  nltts <- create_nltts(
    phylogeny = phylogeny,
    posterior = posterior
  )

  expect_equal("numeric", class(nltts))
  expect_true(all(nltts >= 0.0))
  expect_true(all(nltts <= 1.0))
})
