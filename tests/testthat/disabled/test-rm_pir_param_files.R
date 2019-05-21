context("test-rm_pir_param_files")

test_that("use", {

  if (!beastier::is_on_travis()) return()
  if (!beastier::is_beast2_installed()) return()

  pir_params <- create_test_pir_params(
    experiments = list(create_test_gen_experiment())
  )

  # Files not yet created
  filenames <- c(
    pir_params$alignment_params$fasta_filename,
    pir_params$experiments[[1]]$beast2_options$input_filename,
    pir_params$experiments[[1]]$beast2_options$output_log_filename,
    pir_params$experiments[[1]]$beast2_options$output_trees_filenames,
    pir_params$experiments[[1]]$beast2_options$output_state_filename
  )
  testit::assert(all(!file.exists(filenames)))
  # Evidence files will not be created,
  #   as all models have do_measure_evidence == FALSE
  testit::assert(!file.exists(pir_params$evidence_filename))

  # Running all one experiments
  errors <- pir_run(
    phylogeny = ape::read.tree(text = "((A:2, B:2):1, C:3);"),
    pir_params = pir_params
  )

  # Files exist
  testit::assert(all(file.exists(filenames)))
  # Evidence file will not exist
  testit::assert(!file.exists(pir_params$evidence_filename))

  # Removing the files
  rm_pir_param_files(pir_params)

  # All files should be gone
  expect_true(all(!file.exists(filenames)))
  expect_true(!file.exists(pir_params$evidence_filename))
})
