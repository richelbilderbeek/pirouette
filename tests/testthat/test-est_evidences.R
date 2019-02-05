context("test-est_evidences")

test_that("old skool versus new skool", {

  if (!beastier::is_on_ci()) return()
  if (rappdirs::app_dir()$os == "win") return()

  # Create an alignment
  fasta_filename <- tempfile(fileext = ".fasta")
  sim_alignment_file(
    phylogeny = ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);"),
    alignment_params = create_alignment_params(
      root_sequence = "acgt",
      mutation_rate = 0.1,
      fasta_filename = fasta_filename
    )
  )
  testit::assert(file.exists(fasta_filename))

  # Prepare old skool
  model_select_params <- create_best_model_select_param(
    site_models = list(
      beautier::create_jc69_site_model(),
      beautier::create_hky_site_model()
    ),
    clock_models = list(beautier::create_strict_clock_model()),
    tree_priors = list(beautier::create_yule_tree_prior()),
    epsilon = 100.0
  )
  file.remove(model_select_params$marg_lik_filename)
  testit::assert(!file.exists(model_select_params$marg_lik_filename))

  # Prepare new skool
  experiment_1 <- create_experiment(
    model_type = "candidate",
    run_if = "best_candidate",
    do_measure_evidence = TRUE,
    inference_model = beautier::create_inference_model(
      site_model = beautier::create_jc69_site_model(),
      clock_model = beautier::create_strict_clock_model(),
      tree_prior = beautier::create_yule_tree_prior(),
      mcmc = create_mcmc_nested_sampling(epsilon = 100.0)
    ),
    beast2_options = beastier::create_beast2_options(
      beast2_path = beastier::get_default_beast2_bin_path()
    )
  )
  experiment_2 <- experiment_1
  experiment_2$inference_model$site_model <- beautier::create_hky_site_model()
  experiments <- list(experiment_1, experiment_2)
  check_experiments(experiments)
  beautier::check_inference_model(experiment_1$inference_model)
  beautier::check_inference_model(experiment_2$inference_model)

  # Run old skool
  df_old_skool <- est_evidences(
    fasta_filename = fasta_filename,
    model_select_params = list(model_select_params),
    experiments = list()
  )

  # Run new skool
  df_new_skool <- est_evidences(
    fasta_filename = fasta_filename,
    model_select_params = as.list(seq(1, 314)),
    experiments = experiments
  )
  # Use big tolerance, as there is much stochasticity involved
  expect_equal(df_old_skool, df_new_skool, tolerance = 2.0)
})
