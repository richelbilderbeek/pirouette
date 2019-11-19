test_that("use", {
  expect_true(
    !is.na(
      stringr::str_match(
        string = get_temp_tree_filename(),
        "tree_.*\\.newick"
      )[1, 1]
    )
  )
})
