test_that("use", {

  if (beastier::is_beast2_installed()) {
    expect_silent(check_beast2_installed())
  } else {
    expect_error(
      check_beast2_installed(),
      "BEAST2 not installed. Tip: use 'beastier::install_beast2()'"
    )
  }
})
