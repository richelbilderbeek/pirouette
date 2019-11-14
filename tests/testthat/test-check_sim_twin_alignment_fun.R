test_that("use", {

  expect_silent(
    check_sim_twin_alignment_fun(
      get_copy_true_alignment_fun()
    )
  )

  expect_silent(
    check_sim_twin_alignment_fun(
      get_sim_twin_alignment_with_std_site_model_fun()
    )
  )

  set.seed(42)
  expect_silent(
    check_sim_twin_alignment_fun(
      get_sim_twin_alignment_with_same_n_mutation_fun(
        max_n_tries = 1,
        mutation_rate = 0.5
      )
    )
  )
})


test_that("abuse", {
  expect_error(
    check_sim_twin_alignment_fun(
      sim_twin_alignment_fun = "nonsense"
    ),
    "'sim_twin_alignment_fun' must be a function"
  )

  expect_error(
    check_sim_twin_alignment_fun(
      sim_twin_alignment_fun = function(twin_phylogeny) { } # nolint ignore curly braces placement here
    ),
    "unused arguments .*true_alignment.*root_sequence"
  )
  expect_error(
    check_sim_twin_alignment_fun(
      sim_twin_alignment_fun = function(true_alignment) { } # nolint ignore curly braces placement here
    ),
    "unused arguments .*twin_phylogeny.*root_sequence"
  )
  expect_error(
    check_sim_twin_alignment_fun(
      sim_twin_alignment_fun = function(
        invalid_argument_1 = "irrelevant",
        invalid_argument_2 = "irrelevant"
      ) { "irrelevant" } # nolint ignore curly braces placement here
    ),
    "unused arguments.*twin_phylogeny.*true_alignment"
  )

  expect_error(
    check_sim_twin_alignment_fun(
      sim_twin_alignment_fun = function(
        twin_phylogeny = "irrelevant",
        true_alignment = "irrelevant",
        root_sequence = "irrelevant"
      ) { "not a phylo" } # nolint ignore curly braces placement here
    ),
    "'sim_twin_alignment_fun' must be a function.*returns.*ape::DNAbin"
  )
})
