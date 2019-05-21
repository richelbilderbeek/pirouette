context("test-check_is_ns_beast2_pkg_installed")

test_that("use", {

  if (!beastier::is_on_ci()) return()
  if (rappdirs::app_dir()$os == "win") return()
  if (!beastier::is_beast2_installed()) return()

  # Uninstall
  if (mauricer::is_beast2_pkg_installed("NS")) {
    mauricer::uninstall_beast2_pkg("NS")
  }
  testit::assert(!mauricer::is_beast2_pkg_installed("NS"))

  # Must give error
  expect_error(check_is_ns_beast2_pkg_installed())

  # Install
  mauricer::install_beast2_pkg("NS")

  # Must be silent
  expect_silent(check_is_ns_beast2_pkg_installed())
})
