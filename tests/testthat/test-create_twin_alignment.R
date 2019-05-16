context("test-create_twin_alignment")

test_that("use, twin has more info", {

  true_phylogeny <- ape::read.tree(text = "((A:1, B:1):2, C:3);")
  twin_phylogeny <- ape::read.tree(text = "((A:2, B:2):1, C:3);")
  root_sequence <- create_blocked_dna(1000)
  alignment_params <- create_test_alignment_params(
    root_sequence = root_sequence
  )
  true_alignment <- sim_alignment(
    phylogeny = true_phylogeny,
    alignment_params = alignment_params
  )
  twin_alignment <- create_twin_alignment(
    twin_phylogeny = twin_phylogeny,
    true_alignment = true_alignment,
    alignment_params = alignment_params
  )
  expect_equal(
    count_n_mutations(
      alignment = true_alignment, root_sequence = root_sequence
    ),
    count_n_mutations(
      alignment = twin_alignment, root_sequence = root_sequence
    )
  )
})

test_that("use, twin has less info", {

  true_phylogeny <- ape::read.tree(text = "((A:2, B:2):1, C:3);")
  twin_phylogeny <- ape::read.tree(text = "((A:1, B:1):2, C:3);")
  root_sequence <- create_blocked_dna(1000)
  alignment_params <- create_test_alignment_params(
    root_sequence = root_sequence
  )
  true_alignment <- sim_alignment(
    phylogeny = true_phylogeny,
    alignment_params = alignment_params
  )
  twin_alignment <- create_twin_alignment(
    twin_phylogeny = twin_phylogeny,
    true_alignment = true_alignment,
    alignment_params = alignment_params
  )
  expect_equal(
    count_n_mutations(
      alignment = true_alignment, root_sequence = root_sequence
    ),
    count_n_mutations(
      alignment = twin_alignment, root_sequence = root_sequence
    )
  )
})

test_that("abuse", {

  true_phylogeny <- ape::read.tree(text = "((A:2, B:2):1, C:3);")
  twin_phylogeny <- ape::read.tree(text = "((A:1, B:1):2, C:3);")
  root_sequence <- create_blocked_dna(1000)
  alignment_params <- create_test_alignment_params()
  true_alignment <- sim_alignment(
    phylogeny = true_phylogeny,
    alignment_params = alignment_params
  )
  # Works, just to verify
  expect_silent(
    create_twin_alignment(
      twin_phylogeny = twin_phylogeny,
      true_alignment = true_alignment,
      alignment_params = alignment_params
    )
  )

  # Errors
  expect_error(
    create_twin_alignment(
      twin_phylogeny = "nonsense",
      true_alignment = true_alignment,
      alignment_params = alignment_params
    ),
    "'twin_phylogeny' must be a of class 'phylo'"
  )

  expect_error(
    create_twin_alignment(
      twin_phylogeny = twin_tree,
      true_alignment = "nonsense",
      alignment_params = alignment_params
    ),
    "'true_alignmnent' must be a of class 'DNAbin'"
  )
})
