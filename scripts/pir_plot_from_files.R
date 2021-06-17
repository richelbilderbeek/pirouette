library(pirouette)
library(testthat)

#' Create a \code{pir_out} from a folder
#'
#' @inheritParams default_params_doc
#' @return a \code{pir_out}, as can be checked by \link{check_pir_out}
#' @author Richèl J.C. Bilderbeek
#' @export
create_pir_out_from_folder <- function(
  folder_name
) {
  if (!dir.exists(folder_name)) {
    stop("'folder_name' does not exist. Actual value: ", folder_name)
  }
  t <- tidyr::expand_grid(
    tree = c("true", "twin"),
    inference_model = c("generative", "candidate"),
    inference_model_weight = NA,
    site_model = NA,
    clock_model = NA,
    tree_prior = NA
  )

  t$tree <- as.factor(t$tree)
  t$inference_model <- as.factor(t$inference_model)


  # Inference models
  xml_filenames <- c(
    list.files(path = folder_name, pattern = "^gen.xml$", full.names = TRUE),
    list.files(path = folder_name, pattern = "^best.xml$", full.names = TRUE),
    list.files(path = folder_name, pattern = "^gen_twin.xml$", full.names = TRUE), # nolint indeed a long line
    list.files(path = folder_name, pattern = "^best_twin.xml$", full.names = TRUE) # nolint indeed a long line
  )
  testthat::expect_equal(4, length(xml_filenames))

  inference_models <- tiebeaur::create_inference_models_from_files(
    xml_filenames
  )
  testthat::expect_equal(4, length(inference_models))
  for (i in seq(1, 4)) {
    t$site_model[i] <- inference_models[[i]]$site_model$name
    t$clock_model[i] <- inference_models[[i]]$clock_model$name
    t$tree_prior[i] <- inference_models[[i]]$tree_prior$name
  }
  # Errors
  errors_filenames <- c(
    list.files(path = folder_name, pattern = "^gen_errors.csv$", full.names = TRUE), # nolint indeed a long line
    list.files(path = folder_name, pattern = "^best_errors.csv$", full.names = TRUE), # nolint indeed a long line
    list.files(path = folder_name, pattern = "^gen_errors_twin.csv$", full.names = TRUE), # nolint indeed a long line
    list.files(path = folder_name, pattern = "^best_errors_twin.csv$", full.names = TRUE) # nolint indeed a long line
  )
  testthat::expect_equal(4, length(errors_filenames))

  n_errors <- length(pirouette::read_errors_csv(errors_filenames[1]))
  df_errors <- data.frame(matrix(nrow = 4, ncol = n_errors, data = 0.0))
  colnames(df_errors) <- paste0("error_", 1:n_errors)
  t_errors <- tibble::as_tibble(df_errors)

  for (i in seq_len(4)) {
    t_errors[i, 1:n_errors] <- pirouette::read_errors_csv(errors_filenames[i])
  }

  t <- cbind(t, t_errors)


  t$site_model <- as.factor(t$site_model)
  t$clock_model <- as.factor(t$clock_model)
  t$tree_prior <- as.factor(t$tree_prior)
  t
}

#' Create a \code{pir_out} from a folder
#'
#' @inheritParams default_params_doc
#' @return a \code{pir_out}, as can be checked by \link{check_pir_out}
#' @author Richèl J.C. Bilderbeek
#' @export
create_pir_outs_from_folders <- function(
  folder_names
) {
  pir_outs <- list()

  for (i in seq_along(folder_names)) {
    pir_outs[[i]] <- create_pir_out_from_folder(folder_names[i])
  }

  pir_outs
}



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

  pirouette::pir_plot_from_long(tree_and_model_errors); ggplot2::ggsave(target, width = 7, height = 7)
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

