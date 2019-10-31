test_that("use", {

  expect_silent(
    check_sim_true_alignment_function(
      get_default_sim_true_alignment_function()
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
      sim_true_alignment_function = function(one_too, many_arguments) { }
    ),
    "'sim_true_alignment_function' must be a function with one argument"
  )

  expect_error(
    check_sim_true_alignment_function(
      sim_true_alignment_function = function(invalid_name = "irrelevant") { }
    ),
    "'sim_true_alignment_function' must be a function with one argument called 'true_phylogeny'"
  )

  expect_error(
    check_sim_true_alignment_function(
      sim_true_alignment_function = function(
        true_phylogeny = "irrelevant"
      ) { "not a phylo" }
    ),
    "'sim_true_alignment_function' must be a function that returns an ape::DNAbin"
  )
})
