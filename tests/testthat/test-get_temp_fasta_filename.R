test_that("use", {
  testthat::expect_true(
    !is.na(
      stringr::str_match(
        string = pirouette::get_temp_fasta_filename(),
        "alignment_.*\\.fasta"
      )[1, 1]
    )
  )
})
