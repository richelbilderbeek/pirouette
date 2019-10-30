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
      sim_twin_tree_function = function(one_too, many_arguments) { }
    ),
    "'sim_twin_tree_function' must be a function with one argument"
  )

  expect_error(
    check_sim_twin_tree_function(
      sim_twin_tree_function = function(irrelevant) { "not a phylo" }
    ),
    "'sim_twin_tree_function' must be a function that returns an ape::phylo"
  )
})
