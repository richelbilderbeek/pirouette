test_that("use", {

  skip("WIP")
  folder_names <- c(
    "~/pirouette_example_42/pirouette_example_42/example_42/314",
    "~/pirouette_example_42/pirouette_example_42/example_42/315"
  )
  expect_true(all(dir.exists(folder_names)))

  pir_outs <- create_pir_outs_from_folders(folder_names)
  expect_silent(check_pir_outs(pir_outs))
  pir_plots(pir_outs)})
