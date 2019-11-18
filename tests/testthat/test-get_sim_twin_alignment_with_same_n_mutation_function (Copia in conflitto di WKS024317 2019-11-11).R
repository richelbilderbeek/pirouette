test_that("use", {
  skip("WIP")

  f <- get_sim_twin_alignment_with_same_n_mutation_function()
  check_sim_twin_alignment_function(f)

  alignment <- f(twin_phylogeny = ape::rcoal(3), true_alignment = "irrelevant")
  testthat::expect_silent(pirouette::check_alignment(alignment))
})
