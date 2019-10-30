test_that("use", {

  expect_silent(
    check_sim_twin_alignment_function(
      get_default_sim_twin_alignment_function()
    )
  )
})


test_that("abuse", {
  expect_error(
    check_sim_twin_alignment_function(
      sim_twin_alignment_function = "nonsense"
    ),
    "'sim_twin_alignment_function' must be a function"
  )

  expect_error(
    check_sim_twin_alignment_function(
      sim_twin_alignment_function = function(one_too_few_arguments) { }
    ),
    "'sim_twin_alignment_function' must be a function with two arguments"
  )
  expect_error(
    check_sim_twin_alignment_function(
      sim_twin_alignment_function = function(one, too, many_arguments) { }
    ),
    "'sim_twin_alignment_function' must be a function with two arguments"
  )
  expect_error(
    check_sim_twin_alignment_function(
      sim_twin_alignment_function = function(
        invalid_argument_1 = "irrelevant",
        invalid_argument_2 = "irrelevant"
      ) { "irrelevant" }
    ),
    "'sim_twin_alignment_function' must be a function with two arguments called 'twin_phylogeny' and 'true_alignment'"
  )

  expect_error(
    check_sim_twin_alignment_function(
      sim_twin_alignment_function = function(
        twin_phylogeny = "irrelevant", true_alignment = "irrelevant"
      ) { "not a phylo" }
    ),
    "'sim_twin_alignment_function' must be a function that returns an ape::DNAbin"
  )
})
