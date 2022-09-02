test_that("package style", {
  if (rappdirs::app_dir()$os != "win" && beautier::is_on_ci()) {
    lintr::expect_lint_free()
  }
})
