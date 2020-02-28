test_that("use", {

  if (rappdirs::app_dir()$os == "win") return()

  pir_paramses <- create_std_pir_paramses(n = 2)

  for (i in seq_along(pir_paramses)) {
    for (j in seq_along(pir_params$experiments)) {
      expect_false(
        pir_paramses[[i]]$experiments[[j]]$inference_model$mcmc$chain_length == 3000
      )
      expect_false(
        pir_paramses[[i]]$experiments[[j]]$est_evidence_mcmc$chain_length == 3000
      )
    }
  }
  pir_paramses <- shorten_pir_paramses(pir_paramses)
  for (i in seq_along(pir_paramses)) {
    for (j in seq_along(pir_params$experiments)) {
      expect_true(
        pir_paramses[[i]]$experiments[[j]]$inference_model$mcmc$chain_length == 3000
      )
      expect_true(
        pir_paramses[[i]]$experiments[[j]]$est_evidence_mcmc$chain_length == 3000
      )
    }
  }
})
