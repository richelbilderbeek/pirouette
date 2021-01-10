test_that("use without extension", {
  if (rappdirs::app_dir()$os == "win") return()
  filename <- "firefox"
  created <- to_evidence_filename(filename)
  expected <- "firefox_evidence"
  expect_equal(created, expected)
})

test_that("use", {
  if (rappdirs::app_dir()$os == "win") return()
  filename <- "beast2_output.log"
  created <- to_evidence_filename(filename)
  expected <- "beast2_output_evidence.log"
  expect_equal(created, expected)
})

test_that("use on filesnames with two dots", {
  if (rappdirs::app_dir()$os == "win") return()
  filename <- "example_3_beast2_output.xml.state"
  created <- to_evidence_filename(filename)
  expected <- "example_3_beast2_output_evidence.xml.state"
  expect_equal(expected, created)
})

test_that("convert file correctly from path with dots", {
  if (rappdirs::app_dir()$os == "win") return()
  filename <- "/my/path.with.dots/file.csv" # nolint do use absolute paths in testing
  expected <- "/my/path.with.dots/file_evidence.csv" # nolint do use absolute paths in testing
  created <- to_evidence_filename(filename)
  expect_equal(created, expected)
})

test_that("abuse", {
  if (rappdirs::app_dir()$os == "win") return()
  expect_silent(to_evidence_filename("OK"))
  expect_error(to_evidence_filename(NA), "'filename' must be one string")
  expect_error(to_evidence_filename(NULL), "'filename' must be one string")
  expect_error(to_evidence_filename(c()), "'filename' must be one string")
  expect_error(
    to_evidence_filename(c("a", "b")),
    "'filename' must be one string"
  )
})
