context("test-alignment_params_to_posterior_trees")

test_that("returns a multiPhylo", {

  if (!beastier::is_on_travis()) return()
  if (!beastier::is_beast2_installed()) return()

  alignment_params <- create_test_alignment_params()
  create_alignment_file(
    phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
    alignment_params = alignment_params
  )
  expect_true(file.exists(alignment_params$fasta_filename))

  tracelog_filename <- tempfile(tmpdir = rappdirs::user_cache_dir())
  treelog_filename <- tempfile(tmpdir = rappdirs::user_cache_dir())
  experiment <- create_test_experiment(
    inference_model = create_test_inference_model(
      mcmc = create_test_mcmc(
        tracelog = create_test_tracelog(filename = tracelog_filename),
        treelog = create_test_treelog(filename = treelog_filename)
      )
    )
  )
  expect_true(!file.exists(tracelog_filename))
  expect_true(!file.exists(treelog_filename))

  trees <- alignment_params_to_posterior_trees(
    alignment_params = alignment_params,
    experiment = experiment
  )
  expect_equal("multiPhylo", class(trees))

  expect_true(file.exists(tracelog_filename))
  expect_true(file.exists(treelog_filename))
})
