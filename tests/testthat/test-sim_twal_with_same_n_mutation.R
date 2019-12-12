test_that("this adapater function is a sim_twin_alignment function", {

  pirouette::check_sim_twal_fun(
    sim_twal_with_same_n_mutation
  )
})

test_that("can simulate an alignment", {

  true_alignment <- pirouette::get_test_alignment(sequence_length = 5)

  alignment <- NA
  # Will warn that the sim failed. No worries here.
  suppressWarnings(
    alignment <- sim_twal_with_same_n_mutation(
      twin_phylogeny = ape::read.tree(text = "((A:1, B:1):2, C:3);"),
      true_alignment = true_alignment,
      root_sequence = "aacgt",
      max_n_tries = 1
    )
  )
  testthat::expect_silent(pirouette::check_alignment(alignment))
})
