test_that("this adapater function is a sim_true_alignment function", {

  check_sim_true_alignment_function(
    sim_true_alignment_with_standard_site_model
  )
})

test_that("can simulate an alignment", {

  alignment <- sim_true_alignment_with_standard_site_model(
    true_phylogeny = ape::read.tree(text = "((A:1, B:1):2, C:3);"),
    root_sequence = "aaaa",
    mutation_rate = 0.1
  )
  expect_silent(check_alignment(alignment))
})

