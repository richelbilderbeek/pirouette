test_that("is valid function", {
  skip("WIP")
  expect_silent(
    check_sim_true_alignment_function(
      get_sim_true_alignment_with_linked_node_sub_site_model_function(
        root_sequence = "acgt",
        mutation_rate = 0.1
      )
    )
  )
})

test_that("usage", {
  skip("WIP")
  alignment_params <- create_alignment_params(
    sim_true_alignment_function =
      get_sim_true_alignment_with_linked_node_sub_site_model_function(
      root_sequence = "aaaaaaaa",
      mutation_rate = 0.5
    )
  )
  true_alignment <- pirouette::sim_true_alignment(
    true_phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
    alignment_params = alignment_params
  )
})
