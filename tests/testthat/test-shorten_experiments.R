test_that("use", {

  if (rappdirs::app_dir()$os == "win") return()

  experiments <- list(
    create_gen_experiment(
      inference_model = create_inference_model(
        site_model = create_hky_site_model()
      )
    ),
    create_cand_experiment()
  )
  for (i in seq_along(experiments)) {
    expect_false(
      experiments[[i]]$inference_model$mcmc$chain_length == 2000
    )
    expect_false(
      experiments[[i]]$est_evidence_mcmc$chain_length == 2000
    )
  }
  experiments <- shorten_experiments(experiments)
  for (i in seq_along(experiments)) {
    expect_equal(
      2000,
      experiments[[i]]$inference_model$mcmc$chain_length
    )
    expect_equal(
      2000,
      experiments[[i]]$est_evidence_mcmc$chain_length
    )
  }
})
