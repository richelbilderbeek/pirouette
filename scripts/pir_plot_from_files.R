library(pirouette)
library(testthat)

filename <- "~/pirouette_example_42/pirouette_example_42/errors.png"
expect_true(file.exists(filename))

pir_plot
?pirouette
pir_plot_from_file(filename)

