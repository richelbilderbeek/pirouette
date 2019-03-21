context("test-pir_to_pics")

test_that("use", {

  if (!beastier::is_on_ci()) return()

  phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  pir_params <- create_test_pir_params(
    twinning_params = create_twinning_params()
  )

  if (rappdirs::app_dir()$os != "win") {
    # Also add a best_candidate experiment
    pir_params$experiments[[2]] <- create_experiment(
      inference_conditions = create_inference_conditions(
        model_type = "candidate",
        run_if = "best_candidate",
        do_measure_evidence = TRUE
      ),
      inference_model = create_inference_model(
        mcmc = create_mcmc(chain_length = 2000, store_every = 1000)
      )
    )
  }
  check_experiments(pir_params$experiments)

  pir_out <- pir_run(phylogeny = phylogeny, pir_params = pir_params)

  folder <- tempdir()
  expected_filenames <- c(
    file.path(folder, "true_tree.png"),
    file.path(folder, "true_alignment.png"),
    file.path(folder, "twin_tree.png"),
    file.path(folder, "twin_alignment.png"),
    file.path(folder, "true_posterior_gen.png"),
    file.path(folder, "twin_posterior_gen.png"),
    file.path(folder, "true_error_histogram_gen.png"),
    file.path(folder, "twin_error_histogram_gen.png"),
    file.path(folder, "true_error_violin_gen.png"),
    file.path(folder, "twin_error_violin_gen.png")
  )

  if (rappdirs::app_dir()$os != "win") {
    expected_filenames <- c(
      expected_filenames,
      file.path(folder, "true_posterior_best.png"),
      file.path(folder, "twin_posterior_best.png"),
      file.path(folder, "true_error_histogram_best.png"),
      file.path(folder, "twin_error_histogram_best.png"),
      file.path(folder, "true_error_violin_best.png"),
      file.path(folder, "twin_error_violin_best.png")
    )
  }
  expect_true(all(!file.exists(expected_filenames)))

  pir_to_pics(
    phylogeny = phylogeny,
    pir_params = pir_params,
    folder = folder
  )

  expected_filenames[ !file.exists(expected_filenames) ]

  expect_true(all(file.exists(expected_filenames)))
})
