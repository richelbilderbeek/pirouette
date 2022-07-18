test_that("update in silence", {
  if (!beautier::is_on_gha()) return()
  update_pirouette(upgrade = "always")
  expect_silent(update_pirouette(upgrade = "always"))
})
