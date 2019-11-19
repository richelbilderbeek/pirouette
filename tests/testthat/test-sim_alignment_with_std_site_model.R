test_that("use", {

  alignment <- sim_alignment_with_std_site_model(
    phylogeny = ape::read.tree(text = "((A:1, B:1):2, C:3);"),
    root_sequence = "aaaa",
    mutation_rate = 0.1,
    site_model = beautier::create_jc69_site_model()
  )
  expect_silent(pirouette::check_alignment(alignment))
})
