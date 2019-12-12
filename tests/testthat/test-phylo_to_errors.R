context("test-phylo_to_errors")

test_that("use", {

  if (!beastier::is_on_travis()) return()
  if (!beastier::is_beast2_installed()) return()

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")

  # User will never call 'phylo_to_errors'.
  # A developer, however, needs to initialize the pir_params
  pir_params <- init_pir_params(create_test_pir_params())

  # Create the alignment
  create_tral_file(
    phylogeny = phylogeny,
    alignment_params = pir_params$alignment_params
  )
  beautier::check_file_exists(pir_params$alignment_params$fasta_filename)

  nltts <- phylo_to_errors(
    phylogeny = phylogeny,
    experiment = pir_params$experiments[[1]],
    alignment_params = pir_params$alignment_params
  )

  expect_true(length(nltts) > 0)
  expect_true(all(nltts > 0) & all(nltts < 1))
})
