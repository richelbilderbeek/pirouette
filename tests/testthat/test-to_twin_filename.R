test_that("use without extension", {
  if (rappdirs::app_dir()$os == "win") return()
  filename <- "firefox"
  created <- to_twin_filename(filename)
  expected <- "firefox_twin"
  expect_equal(created, expected)
})

test_that("use", {
  if (rappdirs::app_dir()$os == "win") return()
  file_1 <- "pippo.txt"
  file_twin <- "pippo_twin.txt"
  expect_equal(file_twin, to_twin_filename(file_1))
})

test_that("use on filesnames with two dots", {
  if (rappdirs::app_dir()$os == "win") return()
  filename <- "example_3_beast2_output.xml.state"
  created <- to_twin_filename(filename)
  expected <- "example_3_beast2_output_twin.xml.state"
  expect_equal(expected, created)
})

test_that("convert file correctly from path with dots", {
  if (rappdirs::app_dir()$os == "win") return()
  # See https://github.com/richelbilderbeek/razzo/issues/182
  filename <- "/my/path.with.dots/file.csv" # nolint do use absolute paths in testing
  expected <- "/my/path.with.dots/file_twin.csv" # nolint do use absolute paths in testing
  created <- to_twin_filename(filename)
  expect_equal(created, expected)
})

test_that("abuse", {
  if (rappdirs::app_dir()$os == "win") return()
  expect_silent(to_twin_filename("OK"))
  expect_error(to_twin_filename(Inf), "filename")
  expect_error(to_twin_filename(NULL), "filename")
  expect_error(to_twin_filename(c()), "filename")
  expect_error(to_twin_filename(c("a", "b")), "filename")
  expect_error(
    to_twin_filename(NA),
    "filename"
  )
  expect_error(
    to_twin_filename(""),
    "filename"
  )
})
