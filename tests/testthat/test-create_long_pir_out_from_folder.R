test_that("use", {
  skip("WIP")
  super_folder <- "/home/richel/pirouette_example_32/pirouette_example_32/example_32" # nolint long line indeed
  folder_names <- list.dirs(super_folder)
  folder_names <- folder_names[folder_names != super_folder]
  folder_name <- folder_names[1]


  long_pir_out <- create_long_pir_out_from_folder(folder_name)
  pirouette::check_tree_and_model_errors(long_pir_out)
})
