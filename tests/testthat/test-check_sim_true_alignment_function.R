test_that("use on adapter functions", {

  testthat::expect_silent(
    pirouette::check_sim_true_alignment_function(
      sim_true_alignment_with_std_site_model
    )
  )

  testthat::expect_silent(
    pirouette::check_sim_true_alignment_function(
      sim_true_alignment_with_linked_node_sub_site_model
    )
  )

  testthat::expect_silent(
    pirouette::check_sim_true_alignment_function(
      sim_true_alignment_with_unlinked_node_sub_site_model
    )
  )

})

test_that("use on get_x_functions", {
  testthat::expect_silent(
    pirouette::check_sim_true_alignment_function(
      pirouette::get_sim_true_alignment_with_std_site_model_function()
    )
  )

  testthat::expect_silent(
    pirouette::check_sim_true_alignment_function(
      pirouette::get_sim_true_alignment_with_linked_node_sub_site_model_function()
    )
  )

  testthat::expect_silent(
    pirouette::check_sim_true_alignment_function(
      pirouette::get_sim_true_alignment_with_unlinked_node_sub_site_model_function()
    )
  )
})

test_that("abuse", {
  testthat::expect_error(
    pirouette::check_sim_true_alignment_function(
      sim_true_alignment_function = "nonsense"
    ),
    "'sim_true_alignment_function' must be a function"
  )

  testthat::expect_error(
    pirouette::check_sim_true_alignment_function(
      sim_true_alignment_function = function(
        true_phylogeny = "irrelevant",
        root_sequence = "irrelevant"
      ) { }
    ),
    "'sim_true_alignment_function' must be a function that returns an ape::DNAbin"
  )

  testthat::expect_error(
    pirouette::check_sim_true_alignment_function(
      sim_true_alignment_function = function(invalid_name = "irrelevant") { }
    ),
    "unused argument.*true_phylogeny"
  )
  testthat::expect_error(
    pirouette::check_sim_true_alignment_function(
      sim_true_alignment_function = function(
        true_phylogeny = "irrelevant"
      ) {}
    ),
    "unused argument.*root_sequence"
  )

  testthat::expect_error(
    pirouette::check_sim_true_alignment_function(
      sim_true_alignment_function = function(
        true_phylogeny = "irrelevant",
        root_sequence = "irrelevant"
      ) { "not a phylo" }
    ),
    "'sim_true_alignment_function' must be a function that returns an ape::DNAbin"
  )
})
