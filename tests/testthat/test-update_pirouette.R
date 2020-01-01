test_that("update in silence", {
  if (!beastier::is_on_travis()) return()
  update_pirouette(upgrade = "always")
  expect_silent(update_pirouette(upgrade = "always"))
})
