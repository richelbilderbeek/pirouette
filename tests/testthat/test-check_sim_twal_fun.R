test_that("use", {

  expect_silent(
    check_sim_twal_fun(
      get_copy_tral_fun()
    )
  )

  expect_silent(
    check_sim_twal_fun(
      get_sim_twal_with_std_nsm_fun()
    )
  )

  set.seed(42)
  expect_silent(
    check_sim_twal_fun(
      get_sim_twal_same_n_muts_fun(
        max_n_tries = 1,
        mutation_rate = 0.5
      )
    )
  )
})


test_that("abuse", {
  expect_error(
    check_sim_twal_fun(
      sim_twal_fun = "nonsense"
    ),
    "'sim_twal_fun' must be a function"
  )

  expect_error(
    check_sim_twal_fun(
      sim_twal_fun = function(twin_phylogeny) { } # nolint ignore curly braces placement here
    ),
    "unused arguments .*true_alignment.*root_sequence"
  )
  expect_error(
    check_sim_twal_fun(
      sim_twal_fun = function(true_alignment) { } # nolint ignore curly braces placement here
    ),
    "unused arguments .*twin_phylogeny.*root_sequence"
  )
  expect_error(
    check_sim_twal_fun(
      sim_twal_fun = function(
        invalid_argument_1 = "irrelevant",
        invalid_argument_2 = "irrelevant"
      ) { "irrelevant" } # nolint ignore curly braces placement here
    ),
    "unused arguments.*twin_phylogeny.*true_alignment"
  )

  expect_error(
    check_sim_twal_fun(
      sim_twal_fun = function(
        twin_phylogeny = "irrelevant",
        true_alignment = "irrelevant",
        root_sequence = "irrelevant"
      ) { "not a phylo" } # nolint ignore curly braces placement here
    ),
    "'sim_twal_fun' must be a function.*returns.*ape::DNAbin"
  )
})
