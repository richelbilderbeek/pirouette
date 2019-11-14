test_that("use", {
  expect_true(
    !is.na(
      stringr::str_match(
        string = get_temp_fasta_filename(),
        "tree_.*\\.newick"
      )[1, 1]
    )
  )
})
