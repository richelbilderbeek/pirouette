test_that("use", {
  expect_silent(create_error_measure_params())
})

test_that("errors are stored correctly", {

  if (!beastier::is_beast2_installed()) return()
  if (rappdirs::app_dir()$os == "win") return()

  phylogeny <- ape::read.tree(text = "((A:1, B:1):1, (C:1, D:1):1);")

  alignment_params <- pirouette::create_alignment_params(
    root_sequence = pirouette::create_blocked_dna(length = 100),
    rng_seed = 1
  )
  experiment <- create_test_gen_experiment()
  error_measure_params <- create_error_measure_params()
  pir_params <- create_pir_params(
    alignment_params = alignment_params,
    experiments = list(experiment),
    error_measure_params = error_measure_params
  )

  expect_true(
    length(
      list.files(
        dirname(experiment$errors_filename),
        pattern = basename(experiment$errors_filename)
      )
    ) == 0
  )

  df <- pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  expect_true(
    length(
      list.files(
        dirname(experiment$errors_filename),
        pattern = basename(experiment$errors_filename)
      )
    ) > 0
  )
})

test_that("abuse", {

  # Exact error messages checked by 'check_error_measure_params]
  expect_error(
    create_error_measure_params(
      burn_in_fraction = "nonsense"
    )
  )
  skip("Issue 371, Issue #371")
  # Use 'error_funs' instead of 'error_fun', to allow for more error functions
  expect_error(
    create_error_measure_params(
      error_funs = "nonsense"
    )
  )
})

test_that("allow to add more errors", {

  skip("Issue 371, Issue #371")
  # Use 'error_funs' instead of 'error_fun', to allow for more error functions
  expect_silent(
    create_error_measure_params(
      error_funs = get_nltt_error_fun()
    )
  )
  expect_silent(
    create_error_measure_params(
      error_funs = list(get_nltt_error_fun(), get_nltt_error_fun())
    )
  )
})
