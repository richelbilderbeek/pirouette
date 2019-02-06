context("test-list_model_select_params")

test_that("use", {

  skip("Deprecated, #90")
  if (!beastier::is_on_travis()) return()

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- create_alignment_params(
    root_sequence = create_mono_nuc_dna(length = 4),
    mutation_rate = 0.01
  )
  model_select_params <- create_gen_model_select_param(
    alignment_params = alignment_params,
    tree_prior = beautier::create_bd_tree_prior()
  )
  expect_silent(
    errors <- pir_run(
      phylogeny = phylogeny,
      pir_params = create_pir_params(
        alignment_params = alignment_params, # deprecated
        model_select_params = model_select_params,
        inference_params = create_inference_params(
          mcmc = beautier::create_mcmc(
            chain_length = 2000,
            store_every = 1000
          )
        ),
        error_measure_params = create_error_measure_params()
      )
    )
  )
})
