context("test-create_twin_alignment")

test_that("use, twin has more info", {
  skip("#255")
  true_tree <- ape::read.tree(text = "((A:1, B:1):2, C:3);")
  twin_tree <- ape::read.tree(text = "((A:2, B:2):1, C:3);")
  root_sequence <- create_blocked_dna(1000)
  true_alignment <- sim_alignment(
    phylogeny = true_tree,
    alignment_params = create_test_alignment_params(
      root_sequence = root_sequence
    )
  )
  image(true_alignment)

  twin_alignment <- create_twin_alignment(
    true_phylogeny = true_tree,
    twin_phylogeny = twin_tree,
    true_alignment = true_alignment
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
  skip("#255")
  true_tree <- ape::read.tree(text = "((A:2, B:2):1, C:3);")
  twin_tree <- ape::read.tree(text = "((A:1, B:1):2, C:3);")
  root_sequence <- create_blocked_dna(1000)
  true_alignment <- sim_alignment(
    phylogeny = true_tree,
    alignment_params = create_test_alignment_params(
      root_sequence = root_sequence
    )
  )
  image(true_alignment)

  twin_alignment <- create_twin_alignment(
    true_phylogeny = true_tree,
    twin_phylogeny = twin_tree,
    true_alignment = true_alignment
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
  skip("#255")
  true_tree <- ape::read.tree(text = "((A:2, B:2):1, C:3);")
  twin_tree <- ape::read.tree(text = "((A:1, B:1):2, C:3);")
  root_sequence <- create_blocked_dna(1000)
  true_alignment <- sim_alignment(
    phylogeny = true_tree,
    alignment_params = create_test_alignment_params()
  )
  # Works, just to verify
  expect_silent(
    create_twin_alignment(
      true_phylogeny = true_tree,
      twin_phylogeny = twin_tree,
      true_alignment = true_alignment
    )
  )

  # Errors
  expect_error(
    create_twin_alignment(
      true_phylogeny = "nonsense",
      twin_phylogeny = twin_tree,
      true_alignment = true_alignment
    ),
    "'true_phylogeny' must be a of class 'phylo'"
  )

  expect_error(
    create_twin_alignment(
      true_phylogeny = true_tree,
      twin_phylogeny = "nonsense",
      true_alignment = true_alignment
    ),
    "'twin_phylogeny' must be a of class 'phylo'"
  )

  expect_error(
    create_twin_alignment(
      true_phylogeny = true_tree,
      twin_phylogeny = twin_tree,
      true_alignment = "nonsense"
    ),
    "'true_alignmnent' must be a of class 'DNAbin'"
  )


})
