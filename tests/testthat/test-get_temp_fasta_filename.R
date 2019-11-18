test_that("use", {
  expect_true(
    !is.na(
      stringr::str_match(
        string = get_temp_fasta_filename(),
        "alignment_.*\\.fasta"
      )[1,1]
    )
  )
})
