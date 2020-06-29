test_that("minimal runs", {

  if (!beastier::is_on_travis()) return()
  if (!beastier::is_beast2_installed()) return()

  skip("Takes too long 7")

  pir_paramses <- list()
  pir_paramses[[1]] <- pirouette::create_test_pir_params()
  pir_paramses[[2]] <- pirouette::create_test_pir_params()

  phylogenies <- list()
  phylogenies[[1]] <- ape::read.tree(text = "((A:2, B:2):1, C:3);")
  phylogenies[[2]] <- ape::read.tree(text = "((A:1, B:1):2, C:3);")

  pir_outs <- pirouette::pir_runs(
    phylogenies = phylogenies,
    pir_paramses = pir_paramses
  )
  testit::assert(length(pir_outs) == length(phylogenies))
  for (pir_out in pir_outs) {
    testthat::expect_silent(pirouette::check_pir_out(pir_out))
  }
})
