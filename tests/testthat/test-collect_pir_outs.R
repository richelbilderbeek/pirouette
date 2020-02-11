test_that("minimal runs", {

  if (!beastier::is_on_travis()) return()
  if (!beastier::is_beast2_installed()) return()

  phylogenies <- list()
  phylogenies[[1]] <- ape::read.tree(text = "((A:2, B:2):1, C:3);")
  phylogenies[[2]] <- ape::read.tree(text = "((A:1, B:1):2, C:3);")

  # experiment
  chain_lenght <- 1e4
  store_every <- 1e3
  experiment <- pirouette::create_gen_experiment()
  experiment$inference_model$mcmc$chain_length <- chain_length
  experiment$inference_model$mcmc$store_every <- store_every
  experiment$est_evidence_mcmc$chain_length <- chain_length
  experiment$est_evidence_mcmc$store_every <- store_every
  experiments <- list(experiment)

  # twinning params
  twinning_params <- pirouette::create_twinning_params(
    sim_twin_tree_fun = pirouette::get_sim_bd_twin_tree_fun(),
    sim_twal_fun = pirouette::get_sim_twal_with_std_nsm_fun(
      mutation_rate = pirouette::create_standard_mutation_rate(
        phylogeny = phylogenies[[1]]
      ),
      site_model = beautier::create_jc69_site_model()
    )
  )

  pir_paramses <- list()
  pir_paramses[[2]] <- pir_paramses[[1]] <-
    pirouette::create_test_pir_params(
      twinning_params = twinning_params,
      experiments = experiments
    )

  pir_outs <- pirouette::pir_runs(
    phylogenies = phylogenies,
    pir_paramses = pir_paramses
  )
  testit::assert(length(pir_outs) == length(phylogenies))

  pir_out <- pirouette::collect_pir_outs(pir_outs)
  testthat::expect_true(
    sum(grepl(pattern = "error_", x = colnames(pir_out))) ==
      length(phylogenies) * (chain_lenght / store_every)
  )
})
