test_that("package style", {
  if (rappdirs::app_dir()$os != "win" && is_on_travis()) {
    lintr::expect_lint_free()
  }
})
