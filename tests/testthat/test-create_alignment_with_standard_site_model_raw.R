test_that("use", {

  alignment <- create_alignment_with_standard_site_model_raw(
    phylogeny = ape::read.tree(text = "((A:1, B:1):2, C:3);"),
    root_sequence = "aaaa",
    mutation_rate = 0.1,
    site_model = beautier::create_jc69_site_model()
  )
  testthat::expect_silent(pirouette::check_alignment(alignment))
})
