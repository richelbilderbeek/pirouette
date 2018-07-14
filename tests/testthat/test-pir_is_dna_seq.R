context("pir_is_dna_seq")

test_that("use", {
  expect_true(pirouette:::pir_is_dna_seq("aaaa"))
  expect_true(pirouette:::pir_is_dna_seq("acgt"))
  expect_false(pirouette:::pir_is_dna_seq("AGCT"))
  expect_false(pirouette:::pir_is_dna_seq("xxxx"))
  expect_false(pirouette:::pir_is_dna_seq(""))
})
