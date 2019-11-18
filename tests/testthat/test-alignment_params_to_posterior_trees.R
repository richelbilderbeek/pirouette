context("test-alignment_params_to_posterior_trees")

test_that("returns a multiPhylo", {

  if (!beastier::is_on_travis()) return()
  if (!beastier::is_beast2_installed()) return()

  alignment_params <- create_test_alignment_params()
  pirouette::create_true_alignment_file(
    phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
    alignment_params = alignment_params
  )
  testthat::expect_true(file.exists(alignment_params$fasta_filename))

  tracelog_filename <- tempfile(tmpdir = rappdirs::user_cache_dir())
  treelog_filename <- tempfile(tmpdir = rappdirs::user_cache_dir())
  experiment <- pirouette::create_test_experiment(
    inference_model = beautier::create_test_inference_model(
      mcmc = beautier::create_test_mcmc(
        tracelog = beautier::create_test_tracelog(filename = tracelog_filename),
        treelog = beautier::create_test_treelog(filename = treelog_filename)
      )
    )
  )
  testthat::expect_true(!file.exists(tracelog_filename))
  testthat::expect_true(!file.exists(treelog_filename))

  trees <- pirouette::alignment_params_to_posterior_trees(
    alignment_params = alignment_params,
    experiment = experiment
  )
  testthat::expect_equal("multiPhylo", class(trees))

  testthat::expect_true(file.exists(tracelog_filename))
  testthat::expect_true(file.exists(treelog_filename))
})
