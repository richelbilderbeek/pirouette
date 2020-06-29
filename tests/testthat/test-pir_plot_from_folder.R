test_that("use", {

  skip("WIP")
  filename <- "~/pirouette_example_42/pirouette_example_42/errors.png"
  expect_true(file.exists(filename))
  folder_name <- dirname(filename)
  expect_true(dir.exists(folder_name))

})
