context("test-check_is_ns_beast2_pkg_installed")

test_that("use", {

  if (!beautier::is_on_ci()) return()
  if (rappdirs::app_dir()$os == "win") return()
  if (!beastier::is_beast2_installed()) return()

  # Uninstall
  if (mauricer::is_beast2_ns_pkg_installed()) {
    mauricer::uninstall_beast2_pkg("NS")
  }
  testthat::expect_true(!mauricer::is_beast2_ns_pkg_installed())

  # Must give error
  expect_error(check_is_ns_beast2_pkg_installed())

  # Install
  mauricerinstall::install_beast2_pkg("NS")

  # Must be silent
  expect_silent(check_is_ns_beast2_pkg_installed())
})
