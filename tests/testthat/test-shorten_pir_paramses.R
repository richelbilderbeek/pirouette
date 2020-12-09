test_that("use", {

  if (rappdirs::app_dir()$os == "win") return()

  pir_paramses <- create_std_pir_paramses(n = 2)

  for (pir_params in pir_paramses) {
    for (experiment in pir_params$experiments) {
      expect_false(experiment$inference_model$mcmc$chain_length == 2000)
      expect_false(experiment$est_evidence_mcmc$chain_length == 2000)
    }
  }
  pir_paramses <- shorten_pir_paramses(pir_paramses)
  for (pir_params in pir_paramses) {
    for (experiment in pir_params$experiments) {
      expect_equal(2000, experiment$inference_model$mcmc$chain_length)
      expect_equal(2000, experiment$est_evidence_mcmc$chain_length)
    }
  }
})
