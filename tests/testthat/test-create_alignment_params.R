test_that("use", {

  expect_silent(
    pirouette::create_alignment_params(
      root_sequence = pirouette::create_blocked_dna(length = 8)
    )
  )

})

test_that("sim_tral_fun", {

  # nodeSub is not on CRAN yet
  # expect_silent( # nolint
  #   pirouette::create_alignment_params( # nolint
  #     sim_tral_fun = # nolint
  #       pirouette::get_sim_tral_with_lns_nsm_fun() # nolint
  #   ) # nolint
  # ) # nolint

  expect_silent(
    pirouette::create_alignment_params(
      sim_tral_fun =
        pirouette::get_sim_tral_with_std_nsm_fun()
    )
  )
})
