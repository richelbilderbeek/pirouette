test_that("use, Windows", {
  skip("Takes too long 26")
  n <- 2
  pir_paramses <- create_std_pir_paramses(
    n = n,
    os = "win"
  )
  expect_equal(length(pir_paramses), n)
  expect_silent(check_pir_paramses(pir_paramses))

  expect_true(
    pir_paramses[[1]]$alignment_params$rng_seed !=
    pir_paramses[[2]]$alignment_params$rng_seed
  )
  expect_true(
    pir_paramses[[1]]$alignment_params$fasta_filename !=
    pir_paramses[[2]]$alignment_params$fasta_filename
  )
})

test_that("use", {
  if (rappdirs::app_dir()$os == "win") return()
  skip("Takes too long 27")

  n <- 2
  pir_paramses <- create_std_pir_paramses(n = n)
  expect_equal(length(pir_paramses), n)
  expect_silent(check_pir_paramses(pir_paramses))

  expect_true(
    pir_paramses[[1]]$alignment_params$rng_seed !=
    pir_paramses[[2]]$alignment_params$rng_seed
  )
  expect_true(
    pir_paramses[[1]]$alignment_params$fasta_filename !=
    pir_paramses[[2]]$alignment_params$fasta_filename
  )
})
