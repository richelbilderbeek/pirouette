test_that("use", {
  f <- get_sim_twin_alignment_with_same_n_mutation_function()
  check_sim_twin_alignment_function(f)

  true_alignment <- get_test_alignment()
  root_sequence <- create_blocked_dna(
    get_alignment_sequence_length(true_alignment)
  )

  alignment <- f(
    twin_phylogeny = ape::rcoal(3),
    true_alignment = true_alignment,
    root_sequence = root_sequence
  )
  expect_silent(pirouette::check_alignment(alignment))
})
