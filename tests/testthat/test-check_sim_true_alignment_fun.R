test_that("use on adapter functions", {

  testthat::expect_silent(
    pirouette::check_sim_true_alignment_fun(
      sim_true_alignment_with_std_site_model
    )
  )

  testthat::expect_silent(
    pirouette::check_sim_true_alignment_fun(
      sim_true_alignment_with_lns_site_model
    )
  )

  testthat::expect_silent(
    pirouette::check_sim_true_alignment_fun(
      sim_true_alignment_with_uns_site_model
    )
  )

})

test_that("use on get_x_funs", {
  testthat::expect_silent(
    pirouette::check_sim_true_alignment_fun(
      pirouette::get_sim_true_alignment_with_std_site_model_fun()
    )
  )

  testthat::expect_silent(
    check_sim_true_alignment_fun(
      get_sim_true_alignment_with_lns_site_model_fun()
    )
  )

  testthat::expect_silent(
    pirouette::check_sim_true_alignment_fun(
      pirouette::get_sim_true_alignment_with_uns_site_model_fun()
    )
  )
})

test_that("abuse", {
  testthat::expect_error(
    pirouette::check_sim_true_alignment_fun(
      sim_true_alignment_fun = "nonsense"
    ),
    "'sim_true_alignment_fun' must be a function"
  )

  testthat::expect_error(
    pirouette::check_sim_true_alignment_fun(
      sim_true_alignment_fun = function(
        true_phylogeny = "irrelevant",
        root_sequence = "irrelevant"
      ) { } # nolint ignore curly braces
    ),
    "'sim_true_alignment_fun' must be a function.*returns.*ape::DNAbin"
  )

  testthat::expect_error(
    pirouette::check_sim_true_alignment_fun(
      sim_true_alignment_fun = function(invalid_name = "irrelevant") { } # nolint ignore curly braces placement here
    ),
    "unused argument.*true_phylogeny"
  )
  testthat::expect_error(
    pirouette::check_sim_true_alignment_fun(
      sim_true_alignment_fun = function(
        true_phylogeny = "irrelevant"
      ) {} # nolint ignore curly braces placement here
    ),
    "unused argument.*root_sequence"
  )

  testthat::expect_error(
    pirouette::check_sim_true_alignment_fun(
      sim_true_alignment_fun = function(
        true_phylogeny = "irrelevant",
        root_sequence = "irrelevant"
      ) { "not a phylo" } # nolint ignore curly braces placement here
    ),
    "'sim_true_alignment_fun' must be a function.*returns.*ape::DNAbin"
  )
})
