context("onLoad")

test_that("use", {

  expect_silent(
    pirouette:::.onLoad(
      libname = "irrelevant",
      pkgname = "irrelevant"
    )
  )

})
