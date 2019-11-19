test_that("use", {
  expect_true("yule" %in% get_twin_models())
  expect_true("birth_death" %in% get_twin_models())
  expect_true("copy_true" %in% get_twin_models())
})
