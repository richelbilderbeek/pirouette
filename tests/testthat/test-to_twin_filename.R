context("test-to_twin_filename")

test_that("use", {
  file_1 <- "pippo.txt"
  file_twin <- "pippo_twin.txt"
  expect_equal(file_twin, to_twin_filename(file_1))
})
