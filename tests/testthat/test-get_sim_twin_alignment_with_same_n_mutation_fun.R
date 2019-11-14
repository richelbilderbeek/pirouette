test_that("use", {
  f <- get_sim_twin_alignment_with_same_n_mutation_fun(
    max_n_tries = 1
  )
  check_sim_twin_alignment_fun(f)

  true_alignment <- get_test_alignment()
  root_sequence <- create_blocked_dna(
    get_alignment_sequence_length(true_alignment)
  )

  alignment <- NA

  # Ignore warning: 'sim_alignment_with_n_mutations' tried 1 times,
  # without simulating an alignment with 0 mutations
  suppressWarnings({
    alignment <- f(
      twin_phylogeny = ape::rcoal(3),
      true_alignment = true_alignment,
      root_sequence = root_sequence
    )
  })
  expect_silent(check_alignment(alignment))
})
