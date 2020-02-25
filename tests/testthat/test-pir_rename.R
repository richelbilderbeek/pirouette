test_that("filenames must change", {

  pir_params <- create_test_pir_params(
    twinning_params = create_twinning_params()
  )

  flat_pir_params <- unlist(pir_params)
  filename_indices <- stringr::str_detect(
    string = names(flat_pir_params),
    pattern = "filename"
  )
  filenames_before <- as.character(unlist(flat_pir_params[filename_indices]))

  cache_pattern <- "/.cache/"
  if (rappdirs::app_dir()$os == "win") cache_pattern <- "cache"

  testthat::expect_true(
    all(
      stringr::str_detect(
        string = stats::na.omit(filenames_before),
        pattern = cache_pattern
      )
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
  filenames_after <- as.character(unlist(flat_pir_params[filename_indices]))
  # Should be made to local, e.g.
  # evidence_186c7280c16b.csv                                                   # nolint this is not commented code
  testthat::expect_true(
    all(
      !stringr::str_detect(
        string = stats::na.omit(filenames_after),
        pattern = cache_pattern
      )
    )
  )
})

test_that("use", {

  testthat::expect_silent(
    pir_rename(
      pir_params = create_test_pir_params(),
      rename_fun = get_remove_dir_fun()
    )
  )
  testthat::expect_silent(
    pir_rename(
      pir_params = create_test_pir_params(),
      rename_fun = get_replace_dir_fun()
    )
  )
  testthat::expect_error(
    pir_rename(
      pir_params = create_test_pir_params(),
      rename_fun = "nonsense"
    )
  )
})
