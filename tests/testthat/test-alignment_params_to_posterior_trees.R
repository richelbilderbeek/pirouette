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

  experiment <- create_test_experiment()

  trees <- alignment_params_to_posterior_trees(
    alignment_params = alignment_params,
    experiment = experiment
  )
  expect_equal("multiPhylo", class(trees))

  expect_true(all(file.exists(
      beastier::get_beast2_options_filenames(experiment$beast2_options)
  )))
})
