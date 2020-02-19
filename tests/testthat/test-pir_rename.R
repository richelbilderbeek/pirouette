test_that("use", {

  skip("Now now")

  pir_params <- create_test_pir_params()

  flat_pir_params <- unlist(pir_params)
  filename_indices <- stringr::str_detect(
    string = names(flat_pir_params),
    pattern = "filename"
  )
  filenames_before <- flat_pir_params[filename_indices]

  # At least on Linux, all temp files have such a structure, e.g.
  # /home/richel/.cache/evidence_186c7280c16b.csv                               # nolint this is not commented code
  expect_true(
    stringr::str_detect(
      string = na.omit(filenames_before),
      pattern = "/.cache/"
    )
  )

  # Make all files to be local
  pir_params <- pir_rename(
    pir_params = pir_params,
    rename_fun = get_remove_dir_fun()
  )

  flat_pir_params <- unlist(pir_params)
  filename_indices <- stringr::str_detect(
    string = names(flat_pir_params),
    pattern = "filename"
  )
  filenames_after <- flat_pir_params[filename_indices]
  # Should be made to local, e.g.
  # evidence_186c7280c16b.csv                                                   # nolint this is not commented code
  expect_true(
    stringr::str_detect(
      string = na.omit(filenames_before),
      pattern = "/.cache/"
    )
  )
})

test_that("use", {

  skip("Now now")

  expect_silent(
    pir_rename(
      pir_params = create_test_pir_params(),
      scheme = "local"
    )
  )
  expect_silent(
    pir_rename(
      pir_params = create_test_pir_params(),
      scheme = "tempdir"
    )
  )
  expect_silent(
    pir_rename(
      pir_params = create_test_pir_params(),
      scheme = "cache_dir"
    )
  )
  expect_silent(
    pir_rename(
      pir_params = create_test_pir_params(),
      scheme = "razzo"
    )
  )
  expect_error(
    pir_rename(
      pir_params = create_test_pir_params(),
      scheme = "nonsense"
    )
  )
})
