test_that("this adapater function is a sim_twin_alignment function", {

  check_sim_twin_alignment_function(
    sim_twin_alignment_with_standard_site_model
  )
})

test_that("can simulate an alignmnet", {

  alignment <- sim_twin_alignment_with_standard_site_model(
    twin_phylogeny = ape::read.tree(text = "((A:1, B:1):2, C:3);"),
    root_sequence = "aaaa",
    mutation_rate = 0.1
  )
  expect_silent(check_alignment(alignment))
})

