context("test-phylo_to_posterior")

test_that("use", {

  skip("WIP Giappo")

  phylogeny <- ape::read.tree(text = "(((A:1,B:1):1,C:2):1,D:3);")
  out <- phylo_to_posterior(
    phylogeny = phylogeny,
    alignment_params = create_alignment_params(
      root_sequence = create_blocked_dna(length = 8),
      mutation_rate = 0.1
    ),
    inference_params = create_inference_params(
      mcmc = beautier::create_mcmc(chain_length = 2000)
    )
  )
  testthat::expect_true(class(out$trees) == "multiPhylo")

  phylogeny <- load_tree(tree_model = "mbd", seed = 1)
  out <- phylo_to_posterior(
    phylogeny = phylogeny,
    alignment_params = create_alignment_params(
      root_sequence = create_blocked_dna(length = 8),
      mutation_rate = 0.1
    ),
    inference_params = create_inference_params(
      mcmc = beautier::create_mcmc(chain_length = 2000)
    )
  )
  testthat::expect_true(class(out$trees) == "multiPhylo")
})

test_that("abuse", {

  skip("WIP Giappo")

  phylogeny <- ape::read.tree(text = "(((A:1,B:1):1,C:2):1,D:3);")
  alignment_params <- create_alignment_params(
    root_sequence = create_blocked_dna(length = 4),
    mutation_rate = 0.1
  )

  expect_error(
    phylo_to_posterior(
      phylogeny = phylogeny,
      alignment_params = alignment_params,
      inference_params = "nonsense"
    ),
    "'inference_params' must be a set of inference parameters."
  )

  expect_error(
    phylo_to_posterior(
      phylogeny = phylogeny,
      alignment_params = "nonsense",
      inference_params = create_inference_params()
    ),
    "'alignment_params' must be a set of alignment parameters"
  )
})
