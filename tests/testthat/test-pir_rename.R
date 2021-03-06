test_that("filenames must change", {

  skip("Takes too long 17")

  pir_params <- pirouette::create_test_pir_params(
    twinning_params = pirouette::create_twinning_params()
  )

  flat_pir_params <- unlist(pir_params)
  filename_indices <- stringr::str_detect(
    string = names(flat_pir_params),
    pattern = "filename"
  )
  filenames_before <- as.character(unlist(flat_pir_params[filename_indices]))

  cache_pattern <- "/.cache/"
  if (rappdirs::app_dir()$os == "win") {
    if (all(grepl(
      x = stats::na.omit(filenames_before), pattern = "cache")
    )) {
      cache_pattern <- "cache"
    } else if (all(grepl(
      x = stats::na.omit(filenames_before), pattern = "Cache")
    )) {
      cache_pattern <- "Cache"
    } else {
      cache_pattern <- basename(dirname(filenames_before[[1]]))
    }
  }

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

  skip("Takes too long 18")

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
