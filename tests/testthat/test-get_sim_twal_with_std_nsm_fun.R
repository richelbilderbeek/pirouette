test_that("use", {
  f <- get_sim_twal_with_std_nsm_fun()
  check_sim_twal_fun(f)

  alignment <- f(
    twin_phylogeny = ape::rcoal(3),
    true_alignment = "irrelevant",
    root_sequence = "acgt"
  )
  testthat::expect_silent(pirouette::check_alignment(alignment))
})
