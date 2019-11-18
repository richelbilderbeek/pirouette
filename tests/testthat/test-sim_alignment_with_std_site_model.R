test_that("use", {

  alignment <- sim_alignment_with_std_site_model(
    phylogeny = ape::read.tree(text = "((A:1, B:1):2, C:3);"),
    root_sequence = "aaaa",
    mutation_rate = 0.1,
    site_model = beautier::create_jc69_site_model()
  )
<<<<<<< HEAD:tests/testthat/test-create_alignment_with_standard_site_model_raw.R
  testthat::expect_silent(pirouette::check_alignment(alignment))
=======
  expect_silent(pirouette::check_alignment(alignment))
>>>>>>> b31a67ccf7a115ac420237774dfccbe724a0a7fa:tests/testthat/test-sim_alignment_with_std_site_model.R
})
