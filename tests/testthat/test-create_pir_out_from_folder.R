test_that("use", {

  skip("WIP")
  folder_name <- "~/pirouette_example_42/pirouette_example_42/example_42/314"
  expect_true(dir.exists(folder_name))

  pir_out <- create_pir_out_from_folder(folder_name)
  expect_silent(check_pir_out(pir_out))
  pir_plot(pir_out)
})
