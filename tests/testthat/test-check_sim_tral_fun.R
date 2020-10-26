test_that("use on adapter functions", {

  expect_silent(check_sim_tral_fun(sim_tral_with_std_nsm))

  # nodeSub is not on CRAN yet
  # # This test will call nodeSub, which will call geiger.
  # # The call to geiger will give a message:
  # #
  # # Registered S3 method overwritten by 'geiger'
  # #
  # # To remove 'geiger' from the DESCRIPTION, we simply call
  # # the next function twice.
  # suppressMessages(check_sim_tral_fun(sim_tral_with_lns_nsm))
  # expect_silent(check_sim_tral_fun(sim_tral_with_lns_nsm))
  #
  # expect_silent(check_sim_tral_fun(sim_tral_with_uns_nsm))

})

test_that("use on get_x_funs", {

  expect_silent(check_sim_tral_fun(get_sim_tral_with_std_nsm_fun()))

  # nodeSub is not on CRAN yet
  # expect_silent(check_sim_tral_fun(get_sim_tral_with_lns_nsm_fun()))
  # expect_silent(check_sim_tral_fun(get_sim_tral_with_uns_nsm_fun()))

})

test_that("abuse", {
  expect_error(
    check_sim_tral_fun(
      sim_tral_fun = "nonsense"
    ),
    "'sim_tral_fun' must be a function"
  )

  expect_error(
    check_sim_tral_fun(
      sim_tral_fun = function(
        true_phylogeny = "irrelevant",
        root_sequence = "irrelevant"
      ) { } # nolint ignore curly braces
    ),
    "'sim_tral_fun' must be a function.*returns.*ape::DNAbin"
  )

  expect_error(
    check_sim_tral_fun(
      sim_tral_fun = function(invalid_name = "irrelevant") { } # nolint ignore curly braces placement here
    ),
    "unused argument.*true_phylogeny"
  )
  expect_error(
    check_sim_tral_fun(
      sim_tral_fun = function(
        true_phylogeny = "irrelevant"
      ) {} # nolint ignore curly braces placement here
    ),
    "unused argument.*root_sequence"
  )

  expect_error(
    check_sim_tral_fun(
      sim_tral_fun = function(
        true_phylogeny = "irrelevant",
        root_sequence = "irrelevant"
      ) { "not a phylo" } # nolint ignore curly braces placement here
    ),
    "'sim_tral_fun' must be a function.*returns.*ape::DNAbin"
  )
})
