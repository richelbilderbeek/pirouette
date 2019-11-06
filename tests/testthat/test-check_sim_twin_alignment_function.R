test_that("use", {

  expect_silent(
    check_sim_twin_alignment_function(
      get_copy_true_alignment_function()
    )
  )

  expect_silent(
    check_sim_twin_alignment_function(
      get_sim_twin_alignment_with_standard_site_model_function()
    )
  )

  expect_silent(
    check_sim_twin_alignment_function(
      get_sim_twin_alignment_with_same_n_mutation_function(
        mutation_rate = 0.01
      )
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
    "unused arguments .*true_alignment.*root_sequence"
  )
  expect_error(
    check_sim_twin_alignment_function(
      sim_twin_alignment_function = function(true_alignment) { }
    ),
    "unused arguments .*twin_phylogeny.*root_sequence"
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
        twin_phylogeny = "irrelevant",
        true_alignment = "irrelevant",
        root_sequence = "irrelevant"
      ) { "not a phylo" }
    ),
    "'sim_twin_alignment_function' must be a function that returns an ape::DNAbin"
  )
})
