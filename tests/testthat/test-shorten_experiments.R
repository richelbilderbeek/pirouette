test_that("use", {
  experiments <- list(
    create_gen_experiment(
      inference_model = create_inference_model(
        site_model = create_hky_site_model()
      )
    ),
    create_cand_experiment()
  )
  check_experiments(experiments)
  for (i in seq_along(experiments)) {
    expect_false(
      experiments[[i]]$inference_model$mcmc$chain_length == 3000
    )
    expect_false(
      experiments[[i]]$est_evidence_mcmc$chain_length == 3000
    )
  }
  experiments <- shorten_experiments(experiments)
  check_experiments(experiments)
  for (i in seq_along(experiments)) {
    expect_true(
      experiments[[i]]$inference_model$mcmc$chain_length == 3000
    )
    expect_true(
      experiments[[i]]$est_evidence_mcmc$chain_length == 3000
    )
  }
})
