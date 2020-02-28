test_that("use", {

  pir_out <- create_test_pir_run_output()
  folder_name <- tempfile()
  pir_save(
    phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
    pir_out = pir_out,
    folder_name = folder_name
  )
  expect_true(file.exists(file.path(folder_name, "phylogeny.newick")))
  expect_true(file.exists(file.path(folder_name, "errors.csv")))
  expect_true(file.exists(file.path(folder_name, "errors.png")))
})
