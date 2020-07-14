test_that("raw", {
  n <- 1000
  t <- tibble::tibble(
    tree_and_model = rep(get_tree_and_model_values(), each = n),
    error_value = stats::rnorm(4 * n, 0.5, 0.1)
  )
  t$tree_and_model <- forcats::as_factor(t$tree_and_model)
  expect_silent(pir_plot_from_long(t))
  pir_plot_from_long(t)
})

test_that("00", {
  pir_out <- create_test_pir_run_output(
    add_twin = FALSE,
    add_best = FALSE
  )
  t <- convert_pir_out_to_long(pir_out)
  expect_silent(pir_plot_from_long(t))
  pir_plot_from_long(t)
})

test_that("01", {
  pir_out <- create_test_pir_run_output(
    add_twin = FALSE,
    add_best = TRUE
  )
  t <- convert_pir_out_to_long(pir_out)
  expect_silent(pir_plot_from_long(t))
  pir_plot_from_long(t)
})

test_that("10", {
  pir_out <- create_test_pir_run_output(
    add_twin = TRUE,
    add_best = FALSE
  )
  t <- convert_pir_out_to_long(pir_out)
  expect_silent(pir_plot_from_long(t))
  pir_plot_from_long(t)
})

test_that("11", {
  pir_out <- create_test_pir_run_output(
    add_twin = TRUE,
    add_best = TRUE
  )
  t <- convert_pir_out_to_long(pir_out)
  expect_silent(pir_plot_from_long(t))
  pir_plot_from_long(t)
})



test_that("values from files are correctly placed", {

  # Create four files with known mean values.
  # Read these.
  # Check if the files' means match the plotted means

  super_folder <- tempfile()
  sub_folder_1 <- file.path(super_folder, "123")
  sub_folder_2 <- file.path(super_folder, "456")
  dir.create(sub_folder_1, recursive = TRUE, showWarnings = FALSE)
  dir.create(sub_folder_2, recursive = TRUE, showWarnings = FALSE)
  gen_errors_1_filename <- file.path(sub_folder_1, "gen_errors.csv")
  gen_errors_twin_1_filename <- file.path(sub_folder_1, "gen_errors_twin.csv")
  best_errors_1_filename <- file.path(sub_folder_1, "best_errors.csv")
  best_errors_twin_1_filename <- file.path(sub_folder_1, "best_errors_twin.csv")
  gen_errors_2_filename <- file.path(sub_folder_2, "gen_errors.csv")
  gen_errors_twin_2_filename <- file.path(sub_folder_2, "gen_errors_twin.csv")
  best_errors_2_filename <- file.path(sub_folder_2, "best_errors.csv")
  best_errors_twin_2_filename <- file.path(sub_folder_2, "best_errors_twin.csv")
  # Generative true: median: 0.2
  gen_mean <- 0.2
  write.csv(data.frame(x = rnorm(100, mean = gen_mean, sd = 0.01)), file = gen_errors_1_filename) # nolint long line indeed
  write.csv(data.frame(x = rnorm(100, mean = gen_mean, sd = 0.01)), file = gen_errors_2_filename) # nolint long line indeed

  # Generative twin: median: 0.1
  gen_twin_mean <- 0.1
  write.csv(data.frame(x = rnorm(100, mean = gen_twin_mean, sd = 0.01)), file = gen_errors_twin_1_filename) # nolint long line indeed
  write.csv(data.frame(x = rnorm(100, mean = gen_twin_mean, sd = 0.01)), file = gen_errors_twin_2_filename) # nolint long line indeed

  # Best true: median: 0.4
  best_mean <- 0.4
  write.csv(data.frame(x = rnorm(100, mean = best_mean, sd = 0.01)), file = best_errors_1_filename) # nolint long line indeed
  write.csv(data.frame(x = rnorm(100, mean = best_mean, sd = 0.01)), file = best_errors_2_filename) # nolint long line indeed

  # Best twin: median: 0.3
  best_twin_mean <- 0.3
  write.csv(data.frame(x = rnorm(100, mean = best_twin_mean, sd = 0.01)), file = best_errors_twin_1_filename) # nolint long line indeed
  write.csv(data.frame(x = rnorm(100, mean = best_twin_mean, sd = 0.01)), file = best_errors_twin_2_filename) # nolint long line indeed

  folder_names <- list.dirs(super_folder)
  folder_names <- folder_names[folder_names != super_folder]


  tree_and_model_errors <- create_tree_and_model_errors_from_folders(
    folder_names
  )

  pir_plot_from_long(tree_and_model_errors)
})
