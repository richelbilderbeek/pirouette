test_that("use", {
  if (rappdirs::app_dir()$os == "win") return()
  filenames <- c("a.csv", "b.xml")
  created <- to_twin_filenames(filenames)
  expected <- c(to_twin_filename("a.csv"), to_twin_filename("b.xml"))
  expect_equal(expected, created)
})
