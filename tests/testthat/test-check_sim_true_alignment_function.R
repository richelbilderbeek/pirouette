test_that("use on adapter functions", {

  expect_silent(
    check_sim_true_alignment_function(
      sim_true_alignment_with_standard_site_model
    )
  )

  expect_silent(
    check_sim_true_alignment_function(
      sim_true_alignment_with_linked_node_sub_site_model
    )
  )

  expect_silent(
    check_sim_true_alignment_function(
      sim_true_alignment_with_unlinked_node_sub_site_model
    )
  )

})

test_that("use on get_x_functions", {
  expect_silent(
    check_sim_true_alignment_function(
      get_sim_true_alignment_with_standard_site_model_function()
    )
  )

  expect_silent(
    check_sim_true_alignment_function(
      get_sim_true_alignment_with_linked_node_sub_site_model_function()
    )
  )

  expect_silent(
    check_sim_true_alignment_function(
      get_sim_true_alignment_with_unlinked_node_sub_site_model_function()
    )
  )
})

test_that("abuse", {
  expect_error(
    check_sim_true_alignment_function(
      sim_true_alignment_function = "nonsense"
    ),
    "'sim_true_alignment_function' must be a function"
  )

  expect_error(
    check_sim_true_alignment_function(
      sim_true_alignment_function = function(
        true_phylogeny = "irrelevant",
        root_sequence = "irrelevant"
      ) { }
    ),
    "'sim_true_alignment_function' must be a function that returns an ape::DNAbin"
  )

  expect_error(
    check_sim_true_alignment_function(
      sim_true_alignment_function = function(invalid_name = "irrelevant") { }
    ),
    "unused argument.*true_phylogeny"
  )
  expect_error(
    check_sim_true_alignment_function(
      sim_true_alignment_function = function(
        true_phylogeny = "irrelevant"
      ) {}
    ),
    "unused argument.*root_sequence"
  )

  expect_error(
    check_sim_true_alignment_function(
      sim_true_alignment_function = function(
        true_phylogeny = "irrelevant",
        root_sequence = "irrelevant"
      ) { "not a phylo" }
    ),
    "'sim_true_alignment_function' must be a function that returns an ape::DNAbin"
  )
})
