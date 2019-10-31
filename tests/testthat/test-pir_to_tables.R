context("test-pir_to_tables")

test_that("use, no twinning", {

  if (!beastier::is_on_ci()) return()
  if (!beastier::is_beast2_installed()) return()
  if (rappdirs::app_dir()$os == "win") return()

  phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  pir_params <- create_test_pir_params()

  # Also add a best_candidate experiment
  pir_params$experiments[[2]] <- create_experiment(
    inference_conditions = create_inference_conditions(
      model_type = "candidate",
      run_if = "best_candidate",
      do_measure_evidence = TRUE
    ),
    inference_model = create_inference_model(
      site_model = create_gtr_site_model(),
      mcmc = create_mcmc(chain_length = 2000, store_every = 1000)
    )
  )

  pir_out <- pir_run(phylogeny = phylogeny, pir_params = pir_params)

  folder <- tempdir()
  expected_filenames <- c(
    file.path(folder, "evidence_true.latex")
  )
  expect_true(all(!file.exists(expected_filenames)))

  created_filenames <- pir_to_tables(
    pir_params = pir_params,
    folder = folder
  )

  expect_true(all(file.exists(expected_filenames)))

  # Table content
  expected_filename <- expected_filenames[1]
  latex_text <- readLines(expected_filename)

  expect_true(
    !all(
      is.na(
        stringr::str_match(string = latex_text, pattern = "Site model")
      )
    )
  )
  expect_true(
    !all(
      is.na(
        stringr::str_match(string = latex_text, pattern = "Clock model")
      )
    )
  )
  expect_true(
    !all(
      is.na(
        stringr::str_match(string = latex_text, pattern = "Tree prior")
      )
    )
  )
  expect_true(
    !all(
      is.na(
        stringr::str_match(string = latex_text, pattern = "log\\(evidence\\)")
      )
    )
  )
  expect_true(
    !all(
      is.na(
        stringr::str_match(string = latex_text, pattern = "Weight")
      )
    )
  )
  expect_true(
    !all(
      is.na(
        stringr::str_match(string = latex_text,
          pattern = "log\\(evidence error\\)")
      )
    )
  )
  expect_true(
    !all(
      is.na(
        stringr::str_match(string = latex_text, pattern = "ESS")
      )
    )
  )
})

test_that("use, twinning", {

  if (!beastier::is_on_ci()) return()
  if (!beastier::is_beast2_installed()) return()
  if (rappdirs::app_dir()$os == "win") return()

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
        site_model = create_gtr_site_model(),
        mcmc = create_mcmc(chain_length = 2000, store_every = 1000)
      )
    )
  }
  pir_out <- pir_run(phylogeny = phylogeny, pir_params = pir_params)

  folder <- tempfile()
  expected_filenames <- c()

  if (rappdirs::app_dir()$os != "win") {
    expected_filenames <- c(
      expected_filenames,
      file.path(folder, "evidence_true.latex"),
      file.path(folder, "evidence_twin.latex")
    )
  }
  expect_true(all(!file.exists(expected_filenames)))

  created_filenames <- pir_to_tables(
    pir_params = pir_params,
    folder = folder
  )

  expect_true(all(file.exists(expected_filenames)))


})
