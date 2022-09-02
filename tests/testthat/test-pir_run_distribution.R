test_that("minimal runs", {

  if (!beautier::is_on_github_actions()) return()
  if (!beastier::is_beast2_installed()) return()

  skip("Takes too long 1")

  # Parameters. There will be as many pir_outs as there are parameters.
  pir_paramses <- list()
  pir_paramses[[1]] <- pirouette::create_test_pir_params()
  pir_paramses[[2]] <- pirouette::create_test_pir_params()

  # Generator function, creates a random coalescent tree with 3 extant species
  sim_phylo_fun <- function() {
    ape::rcoal(3)
  }

  pir_outs <- pir_run_distribution(
    sim_phylo_fun = sim_phylo_fun,
    pir_paramses = pir_paramses
  )
  testthat::expect_true(length(pir_outs) == length(pir_paramses))
  for (pir_out in pir_outs) {
    testthat::expect_silent(pirouette::check_pir_out(pir_out))
  }
})

test_that("abuse", {

  pir_paramses <- list()
  pir_paramses[[1]] <- pirouette::create_test_pir_params()

  #
  # sim_phylo_fun: must be a function that returns an ape::phylo
  #
  expect_error(
    pir_run_distribution(
      sim_phylo_fun = "nonsense",
      pir_paramses = pir_paramses
    ),
    "'sim_phylo_fun' must be a function"
  )
  expect_error(
    pir_run_distribution(
      sim_phylo_fun = NA,
      pir_paramses = pir_paramses
    ),
    "'sim_phylo_fun' must be a function"
  )
  expect_error(
    pir_run_distribution(
      sim_phylo_fun = NULL,
      pir_paramses = pir_paramses
    ),
    "'sim_phylo_fun' must be a function"
  )
  expect_error(
    pir_run_distribution(
      sim_phylo_fun = function() "nonsense",
      pir_paramses = pir_paramses
    ),
    "'sim_phylo_fun' must be a function that returns an ape::phylo object"
  )
  expect_error(
    pir_run_distribution(
      sim_phylo_fun = function() NA,
      pir_paramses = pir_paramses
    ),
    "'sim_phylo_fun' must be a function that returns an ape::phylo object"
  )
  expect_error(
    pir_run_distribution(
      sim_phylo_fun = function() NULL,
      pir_paramses = pir_paramses
    ),
    "'sim_phylo_fun' must be a function that returns an ape::phylo object"
  )


  sim_phylo_fun <- function() {
    ape::rcoal(3)
  }

  #
  # pir_paramses: must be a list of pir_params
  #
  expect_error(
    pir_run_distribution(
      sim_phylo_fun = sim_phylo_fun,
      pir_paramses = "nonsense"
    ),
    "'pir_paramses' must be a list of pir_params objects"
  )
  expect_error(
    pir_run_distribution(
      sim_phylo_fun = sim_phylo_fun,
      pir_paramses = NULL
    ),
    "'pir_paramses' must be a list of pir_params objects"
  )
  expect_error(
    pir_run_distribution(
      sim_phylo_fun = sim_phylo_fun,
      pir_paramses = NA
    ),
    "'pir_paramses' must be a list of pir_params objects"
  )
  expect_error(
    pir_run_distribution(
      sim_phylo_fun = sim_phylo_fun,
      pir_paramses = list()
    ),
    "'pir_paramses' must be a list of pir_params objects"
  )
  expect_error(
    pir_run_distribution(
      sim_phylo_fun = sim_phylo_fun,
      pir_paramses = pir_paramses[[1]] # Just a pir_params
    ),
    "'pir_paramses' must be a list of pir_params objects"
  )
})
