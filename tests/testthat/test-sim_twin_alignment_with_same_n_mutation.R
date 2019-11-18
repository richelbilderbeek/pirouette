test_that("this adapater function is a sim_twin_alignment function", {

<<<<<<< HEAD
  pirouette::check_sim_twin_alignment_function(
=======
  pirouette::check_sim_twin_alignment_fun(
>>>>>>> b31a67ccf7a115ac420237774dfccbe724a0a7fa
    sim_twin_alignment_with_same_n_mutation
  )
})

<<<<<<< HEAD
test_that("can simulate an alignmnet", {

  true_alignment <- pirouette::get_test_alignment(sequence_length = 5)

  alignment <- pirouette::sim_twin_alignment_with_same_n_mutation(
    twin_phylogeny = ape::read.tree(text = "((A:1, B:1):2, C:3);"),
    true_alignment = true_alignment,
    root_sequence = "aacgt"
  )

  testthat::expect_silent(pirouette::check_alignment(alignment))
})

=======
test_that("can simulate an alignment", {

  true_alignment <- pirouette::get_test_alignment(sequence_length = 5)

  alignment <- NA
  # Will warn that the sim failed. No worries here.
  suppressWarnings(
    alignment <- sim_twin_alignment_with_same_n_mutation(
      twin_phylogeny = ape::read.tree(text = "((A:1, B:1):2, C:3);"),
      true_alignment = true_alignment,
      root_sequence = "aacgt",
      max_n_tries = 1
    )
  )
  testthat::expect_silent(pirouette::check_alignment(alignment))
})
>>>>>>> b31a67ccf7a115ac420237774dfccbe724a0a7fa
