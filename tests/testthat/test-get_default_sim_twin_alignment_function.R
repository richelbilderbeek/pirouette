test_that("use", {
  f <- get_default_sim_twin_alignment_function()
  check_sim_twin_alignment_function(f)

  alignment <- f(twin_phylogeny = ape::rcoal(3), true_alignment = "irrelevant")
  expect_silent(check_alignment(alignment))
})
