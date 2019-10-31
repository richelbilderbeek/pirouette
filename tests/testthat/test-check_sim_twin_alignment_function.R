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
      sim_twin_alignment_function = function(twin_phylogeny) { }
    ),
    "unused argument .*true_alignment"
  )
  expect_error(
    check_sim_twin_alignment_function(
      sim_twin_alignment_function = function(true_alignment) { }
    ),
    "unused argument .*twin_phylogeny"
  )
  expect_error(
    check_sim_twin_alignment_function(
      sim_twin_alignment_function = function(
        invalid_argument_1 = "irrelevant",
        invalid_argument_2 = "irrelevant"
      ) { "irrelevant" }
    ),
    "unused arguments.*twin_phylogeny.*true_alignment"
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
