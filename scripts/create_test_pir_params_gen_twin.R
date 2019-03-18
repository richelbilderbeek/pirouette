phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")

# Select all experiments with 'run_if' is 'always'
experiment <- create_experiment(
  inference_conditions = create_inference_conditions(
    model_type = "generative",
    run_if = "always",
    do_measure_evidence = FALSE
  ),
  inference_model = create_inference_model(
    mcmc = create_mcmc(chain_length = 3000, store_every = 1000)
  )
)
experiments <- list(experiment)

twinning_params <- create_twinning_params()

pir_params <- create_pir_params(
  alignment_params = create_test_alignment_params(),
  experiments = experiments,
  twinning_params = twinning_params
)
