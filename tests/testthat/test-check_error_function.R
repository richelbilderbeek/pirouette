test_that("use", {
  expect_silent(
    check_error_function(
      get_gamma_error_function()
    )
  )
  expect_silent(
    check_error_function(
      get_nltt_error_function()
    )
  )
})

test_that("abuse", {
  expect_error(
    check_error_function(
      error_function = function(a) {
        a
      }
    ),
    "'error_function' must be a function with at least two arguments"
  )
  expect_error(
    check_error_function(
      error_function = function(tree, trees) {
        1.0 - nLTT::nltts_diff(tree = tree, trees = trees)
      }
    ),
    "'error_function' must be a function that is zero for identical trees"
  )

  # Wrong parameter values
  expect_error(
    check_error_function(
      error_function = "nonsense"
    ),
    "'error_function' must be a function"
  )
  expect_error(
    check_error_function(
      error_function = function(only_one_param) {
        only_one_param
      }
    ),
    "'error_function' must be a function with at least two arguments"
  )
})
