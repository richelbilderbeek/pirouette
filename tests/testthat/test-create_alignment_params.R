test_that("use", {

  expect_silent(
    create_alignment_params(
      root_sequence = create_blocked_dna(length = 8)
    )
  )
})

test_that("sim_true_alignment_fun", {

  expect_silent(
    create_alignment_params(
      sim_true_alignment_fun =
        get_sim_true_alignment_with_linked_node_sub_site_model_fun()
    )
  )

  expect_silent(
    create_alignment_params(
      sim_true_alignment_fun =
        get_sim_true_alignment_with_std_site_model_fun()
    )
  )
})
