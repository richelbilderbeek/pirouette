test_that("use", {
  f <- get_default_sim_true_alignment_function()
  expect_true(is.function(f))
  alignment <- f(ape::rcoal(3))
  expect_equal(class(alignment), "DNAbin")
})
