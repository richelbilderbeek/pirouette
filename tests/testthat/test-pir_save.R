test_that("use", {

  if (!beautier::is_on_ci()) return()
  if (!beastier::is_beast2_installed()) return()

  skip("Takes too long 8")

  phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  pir_params <- create_test_pir_params()
  pir_out <- pir_run(phylogeny = phylogeny, pir_params = pir_params)
  folder_name <- tempfile()
  pir_save(
    phylogeny = phylogeny,
    pir_params = pir_params,
    pir_out = pir_out,
    folder_name = folder_name
  )
  expect_true(file.exists(file.path(folder_name, "phylogeny.newick")))
  expect_true(file.exists(file.path(folder_name, "errors.csv")))
  expect_true(file.exists(file.path(folder_name, "errors.png")))
})
