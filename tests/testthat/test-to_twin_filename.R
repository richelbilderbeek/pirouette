context("test-to_twin_filename")

test_that("use", {
  file_1 <- "pippo.txt"
  file_twin <- "pippo_twin.txt"
  expect_equal(file_twin, to_twin_filename(file_1))
})

test_that("use on filesnames with two dots", {
  skip("#231")
  created <- to_twin_filename("example_3_beast2_output.xml.state")
  expected <- "example_3_beast2_output_twin.xml.state"
  expect_equal(expected, created)
})

