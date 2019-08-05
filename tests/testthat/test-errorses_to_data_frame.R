context("test-errorses_to_data_frame")

test_that("use", {

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")

  pir_params <- create_pir_params(
    alignment_params = create_test_alignment_params(),
    twinning_params = create_twinning_params(),
    experiments = list(create_test_experiment()),
    error_measure_params = create_error_measure_params(),
    evidence_filename = tempfile(fileext = ".csv"),
    verbose = FALSE
  )

  create_alignment_file(
    phylogeny = phylogeny,
    alignment_params = pir_params$alignment_params,
    verbose = FALSE
  )

  errorses <- list()
  errorses[[1]] <- phylo_to_errors(
    phylogeny = phylogeny,
    alignment_params = pir_params$alignment_params,
    error_measure_params = pir_params$error_measure_params,
    experiment = pir_params$experiments[[1]],
    verbose = pir_params$verbose
  )

  df <- errorses_to_data_frame(
    errorses = errorses,
    experiments = list(pir_params$experiments[[1]]),
    marg_liks = create_test_marg_liks(
      site_models = list(create_jc69_site_model()),
      clock_models = list(create_strict_clock_model()),
      tree_priors = list(create_yule_tree_prior())
    )
  )
  expect_true(is.data.frame(df))
  expected_column_names <- c(
    "tree", "inference_model", "inference_model_weight",
    "site_model", "clock_model", "tree_prior",
    "error_1", "error_2", "error_3"
  )
  expect_true(all(expected_column_names %in% names(df)))
})

test_that("abuse", {

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")

  pir_params <- create_pir_params(
    alignment_params = create_test_alignment_params(),
    twinning_params = create_twinning_params(),
    experiments = list(create_test_experiment()),
    error_measure_params = create_error_measure_params(),
    evidence_filename = tempfile(fileext = ".csv"),
    verbose = FALSE
  )

  create_alignment_file(
    phylogeny = phylogeny,
    alignment_params = pir_params$alignment_params,
    verbose = FALSE
  )

  errorses <- list()
  errorses[[1]] <- phylo_to_errors(
    phylogeny = phylogeny,
    alignment_params = pir_params$alignment_params,
    error_measure_params = pir_params$error_measure_params,
    experiment = pir_params$experiments[[1]],
    verbose = pir_params$verbose
  )
  errorses[[2]] <- errorses[[1]][1:2]

  expect_error(
    errorses_to_data_frame(
      errorses = errorses,
      experiments = list(
        pir_params$experiments[[1]], pir_params$experiments[[1]]
      ),
      marg_liks = create_test_marg_liks(
        site_models = list(create_jc69_site_model()),
        clock_models = list(create_strict_clock_model()),
        tree_priors = list(create_yule_tree_prior())
      )
    ),
    "length.*1.*==.*length.*2.*is not TRUE"
  )
})
