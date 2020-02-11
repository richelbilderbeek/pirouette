test_that("minimal runs", {

  if (!beastier::is_beast2_installed()) return()

  pir_outs <- list()
  pir_outs[[1]] <- create_test_pir_run_output(
    add_twin = TRUE,
    add_best = TRUE
  )
  pir_outs[[2]] <- create_test_pir_run_output(
    add_twin = TRUE,
    add_best = TRUE
  )
  pir_outs[[2]]$error_1 <- 2 * pir_outs[[1]]$error_3
  pir_outs[[2]]$error_2 <- 2 * pir_outs[[1]]$error_1
  pir_outs[[2]]$error_3 <- 2 * pir_outs[[1]]$error_2

  pir_out <- pirouette::collect_pir_outs(pir_outs)
  testthat::expect_true(nrow(pir_out) == 4)
  testthat::expect_true(
    sum(grepl(x = colnames(pir_out), "error_")) ==
      sum(grepl(x = colnames(pir_outs[[1]]), "error_")) +
      sum(grepl(x = colnames(pir_outs[[2]]), "error_"))
  )
})
