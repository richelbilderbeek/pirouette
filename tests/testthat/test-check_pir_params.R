context("test-check_pir_params")

test_that("minimal use", {
  expect_silent(check_pir_params(create_test_pir_params()))
})

test_that("use", {

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- create_alignment_params(
    root_sequence = create_mono_nuc_dna(length = 4)
  )
  pir_params <- create_pir_params(
    alignment_params = alignment_params
  )

  # OK
  expect_silent(
    check_pir_params(
      pir_params
    )
  )

  # All elements mus be present
  expect_error(
    check_pir_params(
      pir_params = list()
    ),
    "'alignment_params' must be an element of an 'pir_params'"
  )

  # Wrong alignment_params
  pir_params_2 <- pir_params
  pir_params_2$alignment_params <- "nonsense"
  expect_error(
    check_pir_params(
      pir_params_2
    ),
    "'alignment_params' must be a set of alignment parameters"
  )

  # Wrong error_measure_params
  pir_params_2 <- pir_params
  pir_params_2$error_measure_params <- "nonsense"
  expect_error(
    check_pir_params(
      pir_params_2
    ),
    "'error_measure_params' must be a set of error measurement parameters"
  )

  # Experiments is an empty list
  pir_params_2 <- pir_params
  pir_params_2$experiments <- list()
  expect_error(
    check_pir_params(
      pir_params_2
    ),
    "'experiments' must be one experiment or a list of one or more experiments"
  )

  # Experiments is a string
  pir_params_2 <- pir_params
  pir_params_2$experiments <- "nonsense"
  expect_error(
    check_pir_params(
      pir_params_2
    ),
    "'experiments' must be one experiment or a list of one or more experiments"
  )

  # Wrong evidence_filename
  pir_params_2 <- pir_params
  pir_params_2$evidence_filename <- 123456789
  expect_error(
    check_pir_params(
      pir_params_2
    ),
    "'evidence_filename' must be a string"
  )
  pir_params_2 <- pir_params
  pir_params_2$evidence_filename <- "nonsense"
  expect_error(
    check_pir_params(
      pir_params_2
    ),
    "'evidence_filename' must be a csv filename"
  )

  # Verbose
  pir_params_2 <- pir_params
  pir_params_2$verbose <- NA
  expect_error(
    check_pir_params(
      pir_params_2
    ),
    "'verbose' must be one boolean"
  )
})
