library(pirouette)
library(testthat)


super_folders <- list.dirs(
  "/media/richel/D2B40C93B40C7BEB/pirouette_examples",
  full.names = TRUE,
  recursive = FALSE
)

super_folders <- tail(super_folders, n = 1)
super_folder <- super_folders[1]
super_folder

for (super_folder in super_folders) {

  message(super_folder)

  example_number <- stringr::str_match(super_folder, "[:digit:]{2}$")[, 1]


  replicates_folder <- file.path(super_folder, paste0("example_", example_number))
  expect_true(dir.exists(replicates_folder))

  #super_folder <- "/home/richel/pirouette_example_42/pirouette_example_42/example_42"
  #super_folder <- "/media/richel/D2B40C93B40C7BEB/pirouette_examples/pirouette_example_18/example_18"
  #super_folder <- "/home/richel/pirouette_example_42/pirouette_example_42/example_42"
  #super_folder <- "/home/richel/pirouette_example_32/pirouette_example_32/example_32"

  folder_names <- list.dirs(
    replicates_folder
  )
  folder_names <- folder_names[folder_names != replicates_folder]
  folder_names
  expect_true(all(dir.exists(folder_names)))

  tree_and_model_errors <- create_tree_and_model_errors_from_folders(
    folder_names = folder_names
  )

  target <- stringr::str_replace(
    super_folder,
    pattern = ".*/(pirouette_example_[:digit:]{2})/example_[:digit:]{2}",
    "~/GitHubs/\\1/errors.png"
  )
  expect_true(file.exists(target))

  pir_plot_from_long(tree_and_model_errors) + ggplot2::ggsave(target, width = 7, height = 7)
}
