# By Giovanni Laudanno
# From https://github.com/richelbilderbeek/pirouette/issues/309#issuecomment-518640610
library(pirouette)

chain_length <- 1e6
store_every <- 1e3
phylogeny_example_3 <- ape::read.tree( # taken from example 3
    text = "(((A:8, B:8):1, C:9):1, ((D:8, E:8):1, F:9):1);"
); plot(phylogeny_example_3)

max_seed <- 3
pir_outs <- vector("list", max_seed)
for (seed in 1:max_seed) {
  message(seed)
  twin_seed <- seed

  # generate "true" phylogeny
  true_phylogeny <- phylogeny_example_3
  pir_params <- create_pir_params(
    alignment_params = create_test_alignment_params(
      root_sequence = create_blocked_dna(length = 1000)
    ),
    experiments = list(create_experiment(
      inference_conditions = create_inference_conditions(
        model_type = "generative",
        run_if = "always",
        do_measure_evidence = FALSE
      ),
      inference_model = create_inference_model(
        mcmc = create_mcmc(
          chain_length = chain_length,
          store_every = store_every
        )
      )
    )),
    twinning_params = create_twinning_params(
      rng_seed_twin_tree = twin_seed,
      rng_seed_twin_alignment = twin_seed
    )
  )
  pir_outs[[seed]] <- pir_run(
    phylogeny = true_phylogeny,
    pir_params = pir_params
  )
}

for (seed in seq_along(pir_outs)) {
  temp <- unname(pir_outs[[seed]][grepl(x = colnames(pir_outs[[seed]]), pattern = "error")])
  if (seed == 1) {
    errors <- temp
  } else {
    errors <- cbind(errors, temp)
  }
}
colnames(errors) <- paste0("error_", 1:ncol(errors))
testit::assert(ncol(errors) == max_seed * ncol(temp))
all_errors <- cbind(pir_outs[[seed]][, 1:6], errors)
pir_plot(all_errors)
