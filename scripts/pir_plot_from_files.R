library(pirouette)
library(testthat)

# The main folder that stores all examples
pirouette_examples_folder <- "/media/richel/D2B40C93B40C7BEB/pirouette_examples"
expect_true(dir.exists(pirouette_examples_folder))

pirouette_example_folders <- list.dirs(
  pirouette_examples_folder,
  full.names = TRUE,
  recursive = FALSE
)

# Create figures
for (pirouette_example_folder in pirouette_example_folders) {

  message(pirouette_example_folder)
  testthat::expect_true(dir.exists(pirouette_example_folder))

  example_number <- stringr::str_match(pirouette_example_folder, "[:digit:]{2}$")[, 1]


  replicates_folder <- file.path(pirouette_example_folder, paste0("example_", example_number))
  testthat::expect_true(dir.exists(replicates_folder))

  folder_names <- list.dirs(replicates_folder)

  # Remove 'replicates_folder' itself
  folder_names <- folder_names[folder_names != replicates_folder]
  testthat::expect_true(all(dir.exists(folder_names)))

  tree_and_model_errors <- pirouette::create_tree_and_model_errors_from_folders(
    folder_names = folder_names
  )

  target <- stringr::str_replace(
    replicates_folder,
    pattern = ".*/(pirouette_example_[:digit:]{2})/example_[:digit:]{2}",
    "~/GitHubs/\\1/errors.png"
  )
  testthat::expect_true(file.exists(target))

  pirouette::pir_plot_from_long(tree_and_model_errors) +
    ggplot2::ggsave(target, width = 7, height = 7)
}

# Collect the wall clock times and replicates
tibbles <- list()

for (pirouette_example_folder in pirouette_example_folders) {

  message(pirouette_example_folder)
  testthat::expect_true(dir.exists(pirouette_example_folder))

  example_number <- stringr::str_match(pirouette_example_folder, "[:digit:]{2}$")[, 1]
  message(example_number)

  replicates_folder <- file.path(pirouette_example_folder, paste0("example_", example_number))
  testthat::expect_true(dir.exists(replicates_folder))

  folder_names <- list.dirs(replicates_folder)
  n_replicates <- length(folder_names) - 1

  all_log_files <- list.files(path = pirouette_example_folder, pattern = ".log", full.names = TRUE)
  log_files <- stringr::str_subset(all_log_files, "run_r_script")
  log_file <- tail(log_files, n = 1)
  message(log_files)

  testthat::expect_equal(1, length(log_file))
  text <- readLines(log_file)
  str <- stringr::str_subset(text, "Used walltime.*")
  n_secs <- peregrine::time_str_to_n_sec(str)

  tibbles[[example_number]] <- tibble::tibble(
    example_number = example_number,
    n_secs = n_secs,
    n_replicates = n_replicates
  )


}

walltimes <- dplyr::bind_rows(tibbles)
walltimes$n_mins <- walltimes$n_secs / 60
walltimes$n_hours <- walltimes$n_mins / 60
walltimes$n_days <- walltimes$n_hours / 24

if (1 == 2) {
  walltimes %>% dplyr::select(example_number, n_replicates, n_days) %>% readr::write_csv("~/GitHubs/pirouette_examples/walltimes.csv")
}

