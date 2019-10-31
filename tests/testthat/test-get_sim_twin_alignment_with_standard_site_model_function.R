test_that("use", {
  f <- get_sim_twin_alignment_with_standard_site_model_function()
  check_sim_twin_alignment_function(f)

  alignment <- f(twin_phylogeny = ape::rcoal(3), true_alignment = "irrelevant")
  expect_silent(check_alignment(alignment))
})
