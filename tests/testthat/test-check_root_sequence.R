test_that("use", {
  expect_silent(check_root_sequence("aaaa"))
  expect_error(
    check_root_sequence("nonsense"),
    "'root_sequence' must be one lowercase DNA character string"
  )
  expect_error(check_root_sequence(""))
  expect_error(check_root_sequence(NA))
  expect_error(check_root_sequence(NULL))
  expect_error(check_root_sequence(Inf))
  expect_error(check_root_sequence(c("aaaa", "cccc")))
})
