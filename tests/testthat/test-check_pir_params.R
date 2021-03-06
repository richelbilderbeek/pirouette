test_that("minimal use", {
  expect_silent(check_pir_params(create_test_pir_params()))
})

test_that("use, gen, no twin", {

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
    "'evidence_filename' must be NA"
  )
  pir_params_2 <- pir_params
  pir_params_2$evidence_filename <- "nonsense"
  expect_error(
    check_pir_params(
      pir_params_2
    ),
    "'evidence_filename' must be NA"
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

test_that("use, gen, twin", {

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- create_alignment_params(
    root_sequence = create_mono_nuc_dna(length = 4)
  )
  pir_params <- create_pir_params(
    alignment_params = alignment_params,
    twinning_params = create_twinning_params()

  )

  # OK
  expect_silent(check_pir_params(pir_params))

  # Twinning
  pir_params_bad <- pir_params
  pir_params_bad$twinning_params <- "nonsense"
  expect_error(check_pir_params(pir_params_bad))


})

test_that("evidence_filename only when there are candidates", {
  pir_params <- create_test_pir_params_setup(
    has_candidate = FALSE,
    has_twinning = FALSE
  )
  pir_params$evidence_filename <- "should_be_na.csv"
  expect_error(
    check_pir_params(pir_params),
    "'evidence_filename' must be NA if there is no evidence estimation"
  )

  if (rappdirs::app_dir()$os == "win") return()

  pir_params <- create_test_pir_params_setup(
    has_candidate = TRUE,
    has_twinning = FALSE
  )
  pir_params$evidence_filename <- "wrong.extension"
  expect_error(
    check_pir_params(pir_params),
    "'evidence_filename' must be a csv filename"
  )

  pir_params <- create_test_pir_params_setup(
    has_candidate = TRUE,
    has_twinning = FALSE
  )
  pir_params$evidence_filename <- NA
  expect_error(
    check_pir_params(pir_params),
    "'evidence_filename' must be a string if there is an evidence estimation"
  )


  pir_params <- create_test_pir_params_setup(
    has_candidate = FALSE,
    has_twinning = TRUE
  )
  pir_params$twinning_params$twin_evidence_filename <- "should_be_na.csv"
  expect_error(
    check_pir_params(pir_params),
    paste0(
      "'twinning_params.twin_evidence_filename' must be NA ",
      "if there is no evidence estimation"
    )
  )
  pir_params <- create_test_pir_params_setup(
    has_candidate = TRUE,
    has_twinning = TRUE
  )
  pir_params$twinning_params$twin_evidence_filename <- NA
  expect_error(
    check_pir_params(pir_params),
    paste0(
      "'twinning_params.twin_evidence_filename' must be a string ",
      "if there is an evidence estimation"
    )
  )
})
