test_that("use", {

  # Takes too long..
  if (!beastier::is_on_travis()) return()

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")

  pir_params <- create_test_pir_params_setup()

  # For developers only, using functions used by developers only
  pir_params <- init_pir_params(pir_params)

  create_tral_file(
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

  # Takes too long..
  if (!beastier::is_on_travis()) return()

  if (rappdirs::app_dir()$os == "win") return()

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  pir_params <- create_test_pir_params_setup(
    has_candidate = TRUE
  )

  # For developers only, using functions used by developers only
  pir_params <- init_pir_params(pir_params)

  create_tral_file(
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
  testit::assert(length(errorses[[1]]) != length(errorses[[2]]))

  expect_error(
    errorses_to_data_frame(
      errorses = errorses,
      experiments = pir_params$experiments,
      marg_liks = create_test_marg_liks(
        site_models = list(create_jc69_site_model()),
        clock_models = list(create_strict_clock_model()),
        tree_priors = list(create_yule_tree_prior())
      )
    ),
    "Lengths between errorses differ \\(4 vs 2\\)"
  )
})
