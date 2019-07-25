context("test-pir_plot_from_file")

test_that("use, generative", {

  pir_out_filename <- tempfile(fileext = ".csv")
  url <- "https://raw.githubusercontent.com/richelbilderbeek/pirouette_example_1/master/example_1_314/errors.csv" # nolint indeed a long URL
  utils::download.file(url = url, destfile = pir_out_filename, quiet = TRUE)

  plot <- pir_plot_from_file(pir_out_filename)

  expect_equal(class(plot), c("gg", "ggplot"))
})

test_that("use, best_candidate", {

  pir_out_filename <- tempfile(fileext = ".csv")
  url <- "https://raw.githubusercontent.com/richelbilderbeek/pirouette_example_12/master/example_12_314/errors.csv" # nolint indeed a long URL
  utils::download.file(url = url, destfile = pir_out_filename, quiet = TRUE)

  plot <- pir_plot_from_file(pir_out_filename)

  expect_equal(class(plot), c("gg", "ggplot"))
})

test_that("use, generative + best_candidate", {

  pir_out_filename <- tempfile(fileext = ".csv")
  url <- "https://raw.githubusercontent.com/richelbilderbeek/pirouette_example_5/master/example_5_314/errors.csv" # nolint indeed a long URL
  utils::download.file(url = url, destfile = pir_out_filename, quiet = TRUE)

  plot <- pir_plot_from_file(pir_out_filename)

  expect_equal(class(plot), c("gg", "ggplot"))
})

test_that("use, generative + twinning", {

  pir_out_filename <- tempfile(fileext = ".csv")
  url <- "https://raw.githubusercontent.com/richelbilderbeek/pirouette_example_9/master/example_9_314/errors.csv" # nolint indeed a long URL
  utils::download.file(url = url, destfile = pir_out_filename, quiet = TRUE)

  plot <- pir_plot_from_file(pir_out_filename)

  expect_equal(class(plot), c("gg", "ggplot"))
})

test_that("use, best_candidate + twinning", {

  pir_out_filename <- tempfile(fileext = ".csv")
  url <- "https://raw.githubusercontent.com/richelbilderbeek/pirouette_example_10/master/example_10_314/errors.csv" # nolint indeed a long URL
  utils::download.file(url = url, destfile = pir_out_filename, quiet = TRUE)

  plot <- pir_plot_from_file(pir_out_filename)

  expect_equal(class(plot), c("gg", "ggplot"))
})

test_that("use, generative + best_candidate + twinning", {

  skip("Wait until transition examples 3 <-> 6 is done")
  pir_out_filename <- tempfile(fileext = ".csv")
  url <- "https://raw.githubusercontent.com/richelbilderbeek/pirouette_example_6/master/example_6_314/errors.csv" # nolint indeed a long URL
  utils::download.file(url = url, destfile = pir_out_filename, quiet = TRUE)

  plot <- pir_plot_from_file(pir_out_filename)

  expect_equal(class(plot), c("gg", "ggplot"))
})