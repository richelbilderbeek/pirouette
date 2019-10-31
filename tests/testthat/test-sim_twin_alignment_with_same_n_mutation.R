test_that("this adapater function is a sim_twin_alignment function", {

  skip("WIP")
  check_sim_twin_alignment_function(
    sim_twin_alignment_with_same_n_mutation
  )
})

test_that("can simulate an alignmnet", {

  skip("WIP")
  alignment <- sim_twin_alignment_with_same_n_mutation(
    twin_phylogeny = ape::read.tree(text = "((A:1, B:1):2, C:3);"),
    root_sequence = "aaaa",
    mutation_rate = 0.1
  )
  expect_silent(check_alignment(alignment))
})

