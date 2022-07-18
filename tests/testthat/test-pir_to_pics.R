test_that("use", {

  if (!beautier::is_on_ci()) return()
  if (!beastier::is_beast2_installed()) return()
  if (rappdirs::app_dir()$os == "win") return()

  skip("Takes too long 6")

  phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  pir_params <- create_test_pir_params(
    twinning_params = create_twinning_params()
  )

  if (rappdirs::app_dir()$os != "win") {
    # Also add a best_candidate experiment
    pir_params$experiments[[2]] <- create_test_cand_experiment(
      inference_model = create_test_inference_model(
        site_model = create_hky_site_model()
      )
    )
    pir_params$evidence_filename <- get_temp_evidence_filename()
    pir_params$twinning_params$twin_evidence_filename <-
      get_temp_evidence_filename()
  }

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
  file.remove(expected_filenames)
  expect_true(all(!file.exists(expected_filenames)))

  created_filenames <- pir_to_pics(
    phylogeny = phylogeny,
    pir_params = pir_params,
    folder = folder
  )

  expect_true(all(file.exists(expected_filenames)))
})
