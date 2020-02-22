test_that("use, 1 pir params", {

  skip("Nah, too hard")
  pir_params <- create_test_pir_params()

  flat_pir_params <- unlist(pir_params)
  seed_indices <- stringr::str_detect(
    string = names(flat_pir_params),
    pattern = "seed"
  )
  seeds_as_list <- flat_pir_params[seed_indices]

  pir_params <- renum_rng_seeds(pir_params, 42)
  rng_seeds <- get_rng_seeds(pir_params)
  expect_true("alignment_params" %in% names(rng_seeds))
  expect_true("beast2_options" %in% names(rng_seeds))
  check_rng_seeds(rng_seeds)
  expect_equal(rng_seeds$alignment_params, 42)
  expect_equal(rng_seeds$beast2_options, 42)
})

test_that("use, 2 pir paramses", {

  skip("Nah, too hard")
  pir_paramses <- list(create_test_pir_params(), create_test_pir_params())
  pir_paramses <- renum_rng_seeds(pir_paramses, c(4, 2))
})
