test_that("minimal use, 1 pir paramses, gen only", {

  rng_seed <- 314
  pir_params <- create_test_pir_params_setup(
    has_candidate = FALSE,
    has_twinning = FALSE
  )
  pir_params <- renum_rng_seeds(
    pir_paramses = list(pir_params),
    rng_seeds = c(rng_seed)
  )[[1]]
  expect_equal(pir_params$alignment_params$rng_seed, rng_seed)
  expect_equal(pir_params$experiments[[1]]$beast2_options$rng_seed, rng_seed)
})

test_that("minimal use, 1 pir paramses, gen + twin", {

  rng_seed <- 42
  pir_params <- create_test_pir_params_setup(
    has_candidate = FALSE,
    has_twinning = TRUE
  )
  pir_params <- renum_rng_seeds(
    pir_paramses = list(pir_params),
    rng_seeds = c(rng_seed)
  )[[1]]
  expect_equal(pir_params$alignment_params$rng_seed, rng_seed)
  expect_equal(pir_params$experiments[[1]]$beast2_options$rng_seed, rng_seed)
  expect_equal(pir_params$twinning_params$rng_seed_twin_tree, rng_seed)
  expect_equal(pir_params$twinning_params$rng_seed_twin_alignment, rng_seed)
})

test_that("minimal use, 1 pir paramses, cand", {

  if (rappdirs::app_dir()$os == "win") return()

  rng_seed <- 42
  pir_params <- create_test_pir_params_setup(
    has_candidate = TRUE,
    has_twinning = FALSE
  )
  pir_params <- renum_rng_seeds(
    pir_paramses = list(pir_params),
    rng_seeds = c(rng_seed)
  )[[1]]
  expect_equal(pir_params$alignment_params$rng_seed, rng_seed)
  expect_equal(pir_params$experiments[[1]]$beast2_options$rng_seed, rng_seed)
  expect_equal(pir_params$experiments[[2]]$beast2_options$rng_seed, rng_seed)
})

test_that("minimal use, 1 pir paramses, gen + cand + twin", {

  if (rappdirs::app_dir()$os == "win") return()

  rng_seed <- 3141
  pir_params <- create_test_pir_params_setup(
    has_candidate = TRUE,
    has_twinning = TRUE
  )
  pir_params <- renum_rng_seeds(
    pir_paramses = list(pir_params),
    rng_seeds = c(rng_seed)
  )[[1]]
  expect_equal(pir_params$alignment_params$rng_seed, rng_seed)
  expect_equal(pir_params$experiments[[1]]$beast2_options$rng_seed, rng_seed)
  expect_equal(pir_params$experiments[[2]]$beast2_options$rng_seed, rng_seed)
  expect_equal(pir_params$twinning_params$rng_seed_twin_tree, rng_seed)
  expect_equal(pir_params$twinning_params$rng_seed_twin_alignment, rng_seed)
})

test_that("use, 2 pir paramses", {

  if (rappdirs::app_dir()$os == "win") return()

  pir_paramses <- list(
    create_test_pir_params_setup(FALSE, FALSE),
    create_test_pir_params_setup(TRUE, TRUE)
  )
  rng_seeds <- c(4, 2)
  pir_paramses <- renum_rng_seeds(
    pir_paramses = pir_paramses,
    rng_seeds = rng_seeds
  )

  expect_equal(
    pir_paramses[[1]]$alignment_params$rng_seed, rng_seeds[1]
  )
  expect_equal(
    pir_paramses[[1]]$experiments[[1]]$beast2_options$rng_seed, rng_seeds[1]
  )
  expect_equal(
    pir_paramses[[2]]$alignment_params$rng_seed, rng_seeds[2]
  )
  expect_equal(
    pir_paramses[[2]]$experiments[[1]]$beast2_options$rng_seed, rng_seeds[2]
  )
  expect_equal(
    pir_paramses[[2]]$experiments[[2]]$beast2_options$rng_seed, rng_seeds[2]
  )
  expect_equal(
    pir_paramses[[2]]$twinning_params$rng_seed_twin_tree, rng_seeds[2]
  )
  expect_equal(
    pir_paramses[[2]]$twinning_params$rng_seed_twin_alignment, rng_seeds[2]
  )
})
