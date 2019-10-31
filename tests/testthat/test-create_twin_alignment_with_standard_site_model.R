test_that("use", {

  alignment <- create_twin_alignment_with_standard_site_model(
    twin_phylogeny = ape::read.tree(text = "((A:1, B:1):2, C:3);"),
    root_sequence = "aaaa",
    mutation_rate = 0.1
  )
  expect_silent(check_alignment(alignment))
})

