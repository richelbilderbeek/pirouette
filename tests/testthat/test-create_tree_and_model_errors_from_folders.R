test_that("use", {
  skip("WIP")
  super_folder <- "/home/richel/pirouette_example_32/pirouette_example_32/example_32" # nolint long line indeed
  folder_names <- list.dirs(super_folder)
  folder_names <- folder_names[folder_names != super_folder]
  tree_and_model_errors <- create_tree_and_model_errors_from_folders(folder_names)
  pirouette::check_tree_and_model_errors(tree_and_model_errors)
})
