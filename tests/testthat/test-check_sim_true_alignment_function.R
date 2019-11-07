test_that("use", {

  expect_silent(
    check_sim_true_alignment_function(
      sim_true_alignment_with_standard_site_model
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
      sim_true_alignment_function = function(true_phylogeny = "irrelevant") { }
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
      ) { "not a phylo" }
    ),
    "'sim_true_alignment_function' must be a function that returns an ape::DNAbin"
  )
})
