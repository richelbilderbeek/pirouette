test_that("use, generative", {

  if (!beautier::is_on_github_actions()) return()

  skip("Fails on Travis, passes locally")

  errors_filename <- tempfile(fileext = ".csv")
  url <- "https://raw.githubusercontent.com/richelbilderbeek/pirouette_example_1/master/example_1_314/errors.csv" # nolint indeed a long URL
  utils::download.file(url = url, destfile = errors_filename, quiet = TRUE)
  expect_true(file.exists(errors_filename))


  plot <- pir_plot_from_file(errors_filename)

  expect_equal(class(plot), c("gg", "ggplot"))
})

test_that("use, best_candidate", {

  if (!beautier::is_on_github_actions()) return()

  skip("Issue 377, Issue #377")

  pir_out_filename <- tempfile(fileext = ".csv")
  url <- "https://raw.githubusercontent.com/richelbilderbeek/pirouette_example_12/master/example_12_314/errors.csv" # nolint indeed a long URL
  utils::download.file(url = url, destfile = pir_out_filename, quiet = TRUE)

  plot <- pir_plot_from_file(pir_out_filename)

  expect_equal(class(plot), c("gg", "ggplot"))
})

test_that("use, generative + best_candidate", {

  if (!beautier::is_on_github_actions()) return()

  skip("Takes too long 20")

  pir_out_filename <- tempfile(fileext = ".csv")
  url <- "https://raw.githubusercontent.com/richelbilderbeek/pirouette_example_5/master/example_5_314/errors.csv" # nolint indeed a long URL
  utils::download.file(url = url, destfile = pir_out_filename, quiet = TRUE)

  plot <- pir_plot_from_file(pir_out_filename)

  expect_equal(class(plot), c("gg", "ggplot"))
})

test_that("use, generative + twinning", {

  if (!beautier::is_on_github_actions()) return()

  skip("Takes too long 21")

  pir_out_filename <- tempfile(fileext = ".csv")
  url <- "https://raw.githubusercontent.com/richelbilderbeek/pirouette_example_9/master/example_9_314/errors.csv" # nolint indeed a long URL
  utils::download.file(url = url, destfile = pir_out_filename, quiet = TRUE)

  plot <- pir_plot_from_file(pir_out_filename)

  expect_equal(class(plot), c("gg", "ggplot"))
})

test_that("use, best_candidate + twinning", {

  if (!beautier::is_on_github_actions()) return()

  skip("Takes too long 22")

  pir_out_filename <- tempfile(fileext = ".csv")
  url <- "https://raw.githubusercontent.com/richelbilderbeek/pirouette_example_10/master/example_10_314/errors.csv" # nolint indeed a long URL
  utils::download.file(url = url, destfile = pir_out_filename, quiet = TRUE)

  plot <- pir_plot_from_file(pir_out_filename)

  expect_equal(class(plot), c("gg", "ggplot"))
})

test_that("use, generative + best_candidate + twinning", {

  if (!beautier::is_on_github_actions()) return()

  skip("Takes too long 23")

  pir_out_filename <- tempfile(fileext = ".csv")
  url <- "https://raw.githubusercontent.com/richelbilderbeek/pirouette_example_3/master/example_3_314/errors.csv" # nolint indeed a long URL
  utils::download.file(url = url, destfile = pir_out_filename, quiet = TRUE)

  plot <- pir_plot_from_file(pir_out_filename)

  expect_equal(class(plot), c("gg", "ggplot"))
})
