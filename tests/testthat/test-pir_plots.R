test_that("use FALSE FALSE", {

  suppressPackageStartupMessages(library(ggplot2))
  add_twin <- FALSE
  add_best <- FALSE
  pir_outs <- list()
  pir_outs[[1]] <- create_test_pir_run_output(
    add_twin = add_twin,
    add_best = add_best
  )
  pir_outs[[2]] <- create_test_pir_run_output(
    add_twin = add_twin,
    add_best = add_best
  )
  expect_silent(pir_plots(pir_outs))
})

test_that("use FALSE TRUE", {

  suppressPackageStartupMessages(library(ggplot2))
  add_twin <- FALSE
  add_best <- TRUE
  pir_outs <- list()
  pir_outs[[1]] <- create_test_pir_run_output(
    add_twin = add_twin,
    add_best = add_best
  )
  pir_outs[[2]] <- create_test_pir_run_output(
    add_twin = add_twin,
    add_best = add_best
  )
  expect_silent(pir_plots(pir_outs))
})

test_that("use TRUE FALSE", {

  suppressPackageStartupMessages(library(ggplot2))
  add_twin <- TRUE
  add_best <- FALSE
  pir_outs <- list()
  pir_outs[[1]] <- create_test_pir_run_output(
    add_twin = add_twin,
    add_best = add_best
  )
  pir_outs[[2]] <- create_test_pir_run_output(
    add_twin = add_twin,
    add_best = add_best
  )
  expect_silent(pir_plots(pir_outs))
})

test_that("use TRUE TRUE", {

  suppressPackageStartupMessages(library(ggplot2))
  add_twin <- TRUE
  add_best <- TRUE
  pir_outs <- list()
  pir_outs[[1]] <- create_test_pir_run_output(
    add_twin = add_twin,
    add_best = add_best
  )
  pir_outs[[2]] <- create_test_pir_run_output(
    add_twin = add_twin,
    add_best = add_best
  )
  expect_silent(pir_plots(pir_outs))
})
