context("test-create_alignment_params")

test_that("use", {

  expect_silent(
    create_alignment_params(
      root_sequence = create_blocked_dna(length = 8),
      mutation_rate = 0.1
    )
  )
})
