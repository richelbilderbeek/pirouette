library(pirouette)
library(testthat)


#super_folder <- "/home/richel/pirouette_example_42/pirouette_example_42/example_42"
#super_folder <- "/media/richel/D2B40C93B40C7BEB/pirouette_examples/pirouette_example_18/example_18"
super_folder <- "/home/richel/pirouette_example_42/pirouette_example_42/example_42"

folder_names <- list.dirs(
  super_folder
)
folder_names <- folder_names[folder_names != super_folder]
folder_names
expect_true(all(dir.exists(folder_names)))

message("1: ", Sys.time())
pir_outs <- create_pir_outs_from_folders(folder_names = folder_names)
message("2: ", Sys.time()) # Takes 1 minute and 5 secs

Sys.time()
p <- pir_plots(pir_outs, verbose = TRUE)
p + ggplot2::ggtitle("") + ggplot2::ggsave("example_42.png", width = 7, height = 7)
Sys.time()

