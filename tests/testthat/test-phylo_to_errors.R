context("test-phylo_to_errors")

test_that("use", {

  if (!beastier::is_on_travis()) return()

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")

  # 'phylo_to_errors' expects an alignment file to be present
  alignment_params <- create_test_alignment_params()

  # Create the alignment
  sim_alignment_file(
    phylogeny = phylogeny,
    alignment_params = alignment_params
  )
  testit::assert(file.exists(alignment_params$fasta_filename))

  experiment <- create_experiment(
    inference_model = create_inference_model(
      mcmc = create_mcmc(chain_length = 2000, store_every = 1000)
    )
  )
  experiments <- list(experiment)

  nltts <- phylo_to_errors(
    phylogeny = phylogeny,
    alignment_params = alignment_params,
    inference_params = create_inference_params(
      mcmc = beautier::create_mcmc(chain_length = 2000)
    )
  )

  expect_true(length(nltts) > 0)
  expect_true(all(nltts > 0) & all(nltts < 1))
})
