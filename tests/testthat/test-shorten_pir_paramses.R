test_that("use", {

  if (rappdirs::app_dir()$os == "win") return()

  pir_paramses <- create_std_pir_paramses(n = 2)

  for (pir_params in pir_paramses) {
    for (experiment in pir_params$experiments) {
      expect_false(experiment$inference_model$mcmc$chain_length == 3000)
      expect_false(experiment$est_evidence_mcmc$chain_length == 3000)
    }
  }
  pir_paramses <- shorten_pir_paramses(pir_paramses)
  for (pir_params in pir_paramses) {
    for (experiment in pir_params$experiments) {
      expect_true(experiment$inference_model$mcmc$chain_length == 3000)
      expect_true(experiment$est_evidence_mcmc$chain_length == 3000)
    }
  }
})
