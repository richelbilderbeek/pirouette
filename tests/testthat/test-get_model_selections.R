context("test-get_model_selections")

test_that("use", {

  expect_true("generative" %in% get_model_selections())
  expect_true("most_evidence" %in% get_model_selections())
  expect_false("nonsense" %in% get_model_selections())
})
