test_that("use", {

  pir_out_filename <- tempfile(fileext = ".csv")
  url <- "https://raw.githubusercontent.com/richelbilderbeek/pirouette_example_6/master/example_6/errors.csv" # nolint indeed a long URL
  utils::download.file(url = url, destfile = pir_out_filename)

  expect_silent(
    pir_plot_from_file(pir_out_filename)
  )
})
