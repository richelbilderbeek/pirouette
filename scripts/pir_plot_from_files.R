library(pirouette)
library(testthat)


#super_folder <- "/home/richel/pirouette_example_42/pirouette_example_42/example_42"
#super_folder <- "/media/richel/D2B40C93B40C7BEB/pirouette_examples/pirouette_example_18/example_18"
#super_folder <- "/home/richel/pirouette_example_42/pirouette_example_42/example_42"
super_folder <- "/home/richel/pirouette_example_32/pirouette_example_32/example_32"

folder_names <- list.dirs(
  super_folder
)
folder_names <- folder_names[folder_names != super_folder]
folder_names
expect_true(all(dir.exists(folder_names)))

Sys.time()
tree_and_model_errors <- create_tree_and_model_errors_from_folders(
  folder_names = folder_names
)
p <- pir_plot_from_long(tree_and_model_errors)
p
p <- p  + ggplot2::ggsave("~/example_42.png", width = 7, height = 7)
p
Sys.time()
