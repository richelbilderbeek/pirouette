test_that("use", {
  skip("WIP")
  super_folder <- "/home/richel/pirouette_example_32/pirouette_example_32/example_32" # nolint long line indeed
  folder_names <- list.dirs(super_folder)
  folder_names <- folder_names[folder_names != super_folder]

  long_pir_out <- create_long_pir_out_from_folders(folder_names)

  df_long <- long_pir_out
  testthat::expect_equal(2, ncol(df_long))
  testthat::expect_true("error_value" %in% names(df_long))
  testthat::expect_true("tree_and_model" %in% names(df_long))
  pirouette::check_tree_and_models(df_long$tree_and_model)
})
