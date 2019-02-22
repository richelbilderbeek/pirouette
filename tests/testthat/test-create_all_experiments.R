context("test-create_all_experiments")

test_that("use", {

  if (rappdirs::app_dir()$os == "win") {
    skip("Cannot run on windows")
  }
  all_experiments <- create_all_experiments()
  for (i in seq_along(all_experiments)) {
    experiment <- all_experiments[[i]]
    expect_silent(check_experiment(experiment))
  }
})
