test_that("use on adapter functions", {

  testthat::expect_silent(
    pirouette::check_sim_tral_fun(
      sim_tral_with_std_nsm
    )
  )

  testthat::expect_silent(
    pirouette::check_sim_tral_fun(
      sim_tral_with_lns_nsm
    )
  )

  testthat::expect_silent(
    pirouette::check_sim_tral_fun(
      sim_tral_with_uns_nsm
    )
  )

})

test_that("use on get_x_funs", {
  testthat::expect_silent(
    pirouette::check_sim_tral_fun(
      pirouette::get_sim_tral_with_std_nsm_fun()
    )
  )

  testthat::expect_silent(
    check_sim_tral_fun(
      get_sim_tral_with_lns_nsm_fun()
    )
  )

  testthat::expect_silent(
    pirouette::check_sim_tral_fun(
      pirouette::get_sim_tral_with_uns_nsm_fun()
    )
  )
})

test_that("abuse", {
  testthat::expect_error(
    pirouette::check_sim_tral_fun(
      sim_tral_fun = "nonsense"
    ),
    "'sim_tral_fun' must be a function"
  )

  testthat::expect_error(
    pirouette::check_sim_tral_fun(
      sim_tral_fun = function(
        true_phylogeny = "irrelevant",
        root_sequence = "irrelevant"
      ) { } # nolint ignore curly braces
    ),
    "'sim_tral_fun' must be a function.*returns.*ape::DNAbin"
  )

  testthat::expect_error(
    pirouette::check_sim_tral_fun(
      sim_tral_fun = function(invalid_name = "irrelevant") { } # nolint ignore curly braces placement here
    ),
    "unused argument.*true_phylogeny"
  )
  testthat::expect_error(
    pirouette::check_sim_tral_fun(
      sim_tral_fun = function(
        true_phylogeny = "irrelevant"
      ) {} # nolint ignore curly braces placement here
    ),
    "unused argument.*root_sequence"
  )

  testthat::expect_error(
    pirouette::check_sim_tral_fun(
      sim_tral_fun = function(
        true_phylogeny = "irrelevant",
        root_sequence = "irrelevant"
      ) { "not a phylo" } # nolint ignore curly braces placement here
    ),
    "'sim_tral_fun' must be a function.*returns.*ape::DNAbin"
  )
})
