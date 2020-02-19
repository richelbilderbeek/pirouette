context("test-rm_pir_param_files")

test_that("use", {

  if (!beastier::is_on_travis()) return()
  if (!beastier::is_beast2_installed()) return()

  pir_params <- create_test_pir_params(
    experiments = list(create_test_gen_experiment())
  )

  # Files not yet created
  filenames <- get_pir_params_filenames(pir_params)
  testit::assert(all(!file.exists(filenames)))

  # Running all one experiments
  errors <- pir_run(
    phylogeny = ape::read.tree(text = "((A:2, B:2):1, C:3);"),
    pir_params = pir_params
  )

  # Removing the files
  rm_pir_param_files(pir_params)

  # All files should be gone
  expect_true(all(!file.exists(filenames)))
})
