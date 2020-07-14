test_that("use", {
  pir_params <- create_test_pir_params()
  pir_out <- pir_run(
    phylogeny = create_test_phylogeny(),
    pir_params = pir_params
  )
  errors_filename <- pir_params$experiments[[1]]$errors_filename
  expect_true(file.exists(errors_filename))
  expect_silent(read_errors_csv(errors_filename))
  t <- read_errors_csv(errors_filename)
  expect_is(t, "numeric")
})

test_that("abuse", {
  expect_error(
    read_errors_csv("abs.ent"),
    "'errors_filename' not found"
  )
})
