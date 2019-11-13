test_that("use", {

  expect_silent(
    check_sim_twin_tree_function(
      create_sim_bd_twin_tree_function()
    )
  )
})


test_that("abuse", {
  expect_error(
    check_sim_twin_tree_function(
      sim_twin_tree_function = "nonsense"
    ),
    "'sim_twin_tree_function' must be a function"
  )

  expect_error(
    check_sim_twin_tree_function(
      sim_twin_tree_function = function(one_too, many_arguments) { } # nolint ignore curly braces placement here
    ),
    "'sim_twin_tree_function' must be a function with one argument"
  )

  expect_error(
    check_sim_twin_tree_function(
      sim_twin_tree_function = function(invalid_argument = "irrelevant") {
        ape::rcoal(3)
      }
    ),
    paste0(
      "'sim_twin_tree_function' must be a function with one argument",
      " called 'true_phylogeny'"
    )
  )

  expect_error(
    check_sim_twin_tree_function(
      sim_twin_tree_function = function(true_phylogeny = "irrelevant") {
        "not a phylo"
      }
    ),
    "'sim_twin_tree_function' must be a function that returns an ape::phylo"
  )
})
