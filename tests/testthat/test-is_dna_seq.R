context("test-is_dna_seq")

test_that("use", {
  expect_true(is_dna_seq("aaaa"))
  expect_true(is_dna_seq("acgt"))
  expect_false(is_dna_seq("AGCT"))
  expect_false(is_dna_seq("xxxx"))
  expect_false(is_dna_seq(""))
})
