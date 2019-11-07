test_that("use", {

  expect_silent(
    create_alignment_params(
      root_sequence = create_blocked_dna(length = 8),
      mutation_rate = 0.1
    )
  )
})

test_that("sim_true_alignment_function", {

  expect_silent(
    create_alignment_params(
      sim_true_alignment_function =
        get_sim_true_alignment_with_linked_node_sub_site_model_function()
    )
  )

  expect_silent(
    create_alignment_params(
      sim_true_alignment_function =
        get_sim_true_alignment_with_standard_site_model_function()
    )
  )
})
