test_that("use", {

  testthat::expect_silent(
    pirouette::create_alignment_params(
      root_sequence = pirouette::create_blocked_dna(length = 8)
    )
  )

})

test_that("sim_true_alignment_fun", {

  testthat::expect_silent(
    pirouette::create_alignment_params(
      sim_true_alignment_fun =
        pirouette::get_sim_true_alignment_with_lns_site_model_fun()
    )
  )

  testthat::expect_silent(
    pirouette::create_alignment_params(
      sim_true_alignment_fun =
        pirouette::get_sim_true_alignment_with_std_site_model_fun()
    )
  )

})
