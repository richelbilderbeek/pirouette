test_that("values from files are correctly placed", {

  # Create four files with known mean values.
  # Read these.
  # Check if the files' means match the parsed means


  folder_name <- tempfile()
  dir.create(folder_name, recursive = TRUE, showWarnings = FALSE)
  gen_errors_filename <- file.path(folder_name, "gen_errors.csv")
  gen_errors_twin_filename <- file.path(folder_name, "gen_errors_twin.csv")
  best_errors_filename <- file.path(folder_name, "best_errors.csv")
  best_errors_twin_filename <- file.path(folder_name, "best_errors_twin.csv")
  # Generative true: median: 0.2
  gen_mean <- 0.2
  write.csv(data.frame(x = rnorm(100, mean = gen_mean, sd = 0.01)), file = gen_errors_filename) # nolint long line indeed

  # Generative twin: median: 0.1
  gen_twin_mean <- 0.1
  write.csv(data.frame(x = rnorm(100, mean = gen_twin_mean, sd = 0.01)), file = gen_errors_twin_filename) # nolint long line indeed

  # Best true: median: 0.4
  best_mean <- 0.4
  write.csv(data.frame(x = rnorm(100, mean = best_mean, sd = 0.01)), file = best_errors_filename) # nolint long line indeed

  # Best twin: median: 0.3
  best_twin_mean <- 0.3
  write.csv(data.frame(x = rnorm(100, mean = best_twin_mean, sd = 0.01)), file = best_errors_twin_filename) # nolint long line indeed

  tree_and_model_errors <- create_tree_and_model_errors_from_folder(
    folder_name
  )

  means <- tree_and_model_errors %>%
    dplyr::group_by(tree_and_model) %>%
    dplyr::summarise(mean = mean(error_value), .groups = "keep")

  expect_equal(
    means[means$tree_and_model == "true_generative", ]$mean,
    gen_mean,
    tolerance = 0.01
  )
  expect_equal(
    means[means$tree_and_model == "twin_generative", ]$mean,
    gen_twin_mean,
    tolerance = 0.01
  )
  expect_equal(
    means[means$tree_and_model == "true_candidate", ]$mean,
    best_mean,
    tolerance = 0.01
  )
  expect_equal(
    means[means$tree_and_model == "twin_candidate", ]$mean,
    best_twin_mean,
    tolerance = 0.01
  )
})

test_that("abuse", {
  expect_error(
    create_tree_and_model_errors_from_folder(
      folder_name = "absent"
    ),
    "Not all four files found in folder"
  )
})
