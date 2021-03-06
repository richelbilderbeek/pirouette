test_that("this adapater function is a sim_twin_alignment function", {

  check_sim_twal_fun(
    sim_twal_with_std_nsm
  )
})

test_that("can simulate an alignmnet", {

  alignment <- sim_twal_with_std_nsm(
    twin_phylogeny = ape::read.tree(text = "((A:1, B:1):2, C:3);"),
    root_sequence = "aaaa",
    mutation_rate = 0.1
  )
  testthat::expect_silent(pirouette::check_alignment(alignment))
})
