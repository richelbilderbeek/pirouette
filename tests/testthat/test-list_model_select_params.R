context("test-list_model_select_params")

test_that("use", {
  model_select_params <- create_gen_model_select_param(
    alignment_params = alignment_params,
    tree_prior = beautier::create_bd_tree_prior()
  )
  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- create_alignment_params(
    mutation_rate = 0.01
  )
  expect_silent(
    errors <- pir_run(
      phylogeny = phylogeny,
      alignment_params = alignment_params,
      model_select_params = model_select_params,
      inference_param = create_inference_param(
        mcmc = beautier::create_mcmc(
          chain_length = 2000,
          store_every = 1000
        )
      )
    )
  )
})
