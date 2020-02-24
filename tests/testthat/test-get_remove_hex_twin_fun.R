test_that("minimal use", {
  expect_silent(get_remove_hex_twin_fun())
})

test_that("correct behaviour", {
  f <- get_remove_hex_twin_fun()
  expect_equal(
    f("/home/richel/.cache/beast2_186c7404208c_twin.xml.state"),
    "/home/richel/.cache/beast2_twin.xml.state"
  )
  expect_equal(
    f("beast2_186c7404208c_twin.xml.state"),
    "beast2_twin.xml.state"
  )
  expect_equal(
    f(NA),
    NA
  )
  expect_error(f(c()))
})
