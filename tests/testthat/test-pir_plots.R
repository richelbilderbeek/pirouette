test_that("use FALSE FALSE", {

  suppressPackageStartupMessages(library(ggplot2))
  pir_outs <- list()
  pir_outs[[1]] <- create_test_pir_run_output(
    add_twin = FALSE,
    add_best = FALSE
  )
  pir_outs[[2]] <- pir_outs[[1]]
  expect_silent(pir_plots(pir_outs))
})

test_that("use FALSE TRUE", {

  pir_outs <- list()
  pir_outs[[1]] <- create_test_pir_run_output(
    add_twin = FALSE,
    add_best = TRUE
  )
  pir_outs[[2]] <- pir_outs[[1]]
  expect_silent(pir_plots(pir_outs))
})

test_that("use TRUE FALSE", {

  pir_outs <- list()
  pir_outs[[1]] <- create_test_pir_run_output(
    add_twin = TRUE,
    add_best = FALSE
  )
  pir_outs[[2]] <- pir_outs[[1]]
  expect_silent(pir_plots(pir_outs))
})

test_that("use TRUE TRUE", {

  pir_outs <- list()
  pir_outs[[1]] <- create_test_pir_run_output(
    add_twin = TRUE,
    add_best = TRUE
  )
  pir_outs[[2]] <- pir_outs[[1]]
  expect_silent(pir_plots(pir_outs))
})
