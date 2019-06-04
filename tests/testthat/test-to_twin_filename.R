context("test-to_twin_filename")

test_that("use", {
  file_1 <- "pippo.txt"
  file_twin <- "pippo_twin.txt"
  expect_equal(file_twin, to_twin_filename(file_1))
})

test_that("use on filesnames with two dots", {
  filename <- "example_3_beast2_output.xml.state"
  created <- to_twin_filename(filename)
  expected <- "example_3_beast2_output_twin.xml.state"
  expect_equal(expected, created)
})

test_that("convert file correctly from path with dots", {
  # See https://github.com/richelbilderbeek/razzo/issues/182
  filename <- "/my/path.with.dots/file.csv"
  expected <- "/my/path.with.dots/file_twin.csv"
  created <- to_twin_filename(filename)
  expect_equal(created, expected)
})
