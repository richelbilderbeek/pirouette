test_that("use", {

  if (!beastier::is_on_ci()) return()

  expect_silent(install_pir_deps())

  expect_message(
    remotes::install_github("richelbilderbeek/mcbette", dependencies = TRUE),
    paste0(
      "Skipping install of \'mcbette\' from a github remote, ",
      "the SHA1 \\(.*\\) has not changed since last install"
    )
  )
  expect_true(beastier::is_beast2_installed())
  expect_true(mauricer::is_beast2_png_installed("NS"))

})
