context("test-alignment_params_to_posterior_trees")

test_that("abuse", {

  expect_error(
    alignment_params_to_posterior_trees(
      alignment_params = create_alignment_params(
        root_sequence = create_mono_nuc_dna(length = 4),
        mutation_rate = 1
      ),
      inference_model = create_old_skool_inference_model(
        site_model = beautier::create_jc69_site_model(),
        clock_model = beautier::create_strict_clock_model(),
        tree_prior = beautier::create_tree_prior_bd()
      ),
      inference_params = "nonsense"
    )
  )
})
