test_that("use", {

  if (rappdirs::app_dir()$os == "win") return()

  pir_params <- create_std_pir_params()
  for (i in seq_along(pir_params$experiments)) {
    expect_false(
      pir_params$experiments[[i]]$inference_model$mcmc$chain_length == 2000
    )
    expect_false(
      pir_params$experiments[[i]]$est_evidence_mcmc$chain_length == 2000
    )
  }
  pir_params <- shorten_pir_params(pir_params)
  for (i in seq_along(pir_params$experiments)) {
    expect_equal(
      2000,
      pir_params$experiments[[i]]$inference_model$mcmc$chain_length
    )
    expect_equal(
      2000,
      pir_params$experiments[[i]]$est_evidence_mcmc$chain_length
    )
  }
})
