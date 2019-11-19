test_that("use", {
  expect_silent(
    check_error_fun(
      get_gamma_error_fun()
    )
  )
  expect_silent(
    check_error_fun(
      get_nltt_error_fun()
    )
  )
})

test_that("abuse", {
  expect_error(
    check_error_fun(
      error_fun = function(a) {
        a
      }
    ),
    "'error_fun' must be a function with at least two arguments"
  )
  expect_error(
    check_error_fun(
      error_fun = function(tree, trees) {
        1.0 - nLTT::nltts_diff(tree = tree, trees = trees)
      }
    ),
    "'error_fun' must be a function that is zero for identical trees"
  )

  # Wrong parameter values
  expect_error(
    check_error_fun(
      error_fun = "nonsense"
    ),
    "'error_fun' must be a function"
  )
  expect_error(
    check_error_fun(
      error_fun = function(only_one_param) {
        only_one_param
      }
    ),
    "'error_fun' must be a function with at least two arguments"
  )
})
