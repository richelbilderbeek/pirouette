test_that("use", {
  f <- pirouette::get_sim_twin_alignment_with_std_site_model_fun()
  pirouette::check_sim_twin_alignment_fun(f)

  alignment <- f(
    twin_phylogeny = ape::rcoal(3),
    true_alignment = "irrelevant",
    root_sequence = "acgt"
  )
  testthat::expect_silent(pirouette::check_alignment(alignment))
})
