context("test-create_twin_alignment")

test_that("use, twin has more info", {

  true_phylogeny <- ape::read.tree(text = "((A:1, B:1):2, C:3);")
  twin_phylogeny <- ape::read.tree(text = "((A:2, B:2):1, C:3);")
  root_sequence <- create_blocked_dna(1000)
  alignment_params <- create_test_alignment_params(
    root_sequence = root_sequence
  )
  twinning_params <- create_twinning_params()
  true_alignment <- sim_alignment(
    phylogeny = true_phylogeny,
    alignment_params = alignment_params
  )
  twin_alignment <- create_twin_alignment(
    twin_phylogeny = twin_phylogeny,
    true_alignment = true_alignment,
    alignment_params = alignment_params,
    twinning_params = twinning_params
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
  twinning_params <- create_twinning_params()
  twin_alignment <- create_twin_alignment(
    twin_phylogeny = twin_phylogeny,
    true_alignment = true_alignment,
    alignment_params = alignment_params,
    twinning_params = twinning_params
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
  twinning_params <- create_twinning_params()
  # Works, just to verify
  expect_silent(
    create_twin_alignment(
      twin_phylogeny = twin_phylogeny,
      true_alignment = true_alignment,
      alignment_params = alignment_params,
      twinning_params = twinning_params
    )
  )

  # Errors
  expect_error(
    create_twin_alignment(
      twin_phylogeny = "nonsense",
      true_alignment = true_alignment,
      alignment_params = alignment_params,
      twinning_params = twinning_params
    ),
    "'twin_phylogeny' must be a of class 'phylo'"
  )

  expect_error(
    create_twin_alignment(
      twin_phylogeny = twin_phylogeny,
      true_alignment = "nonsense",
      alignment_params = alignment_params,
      twinning_params = twinning_params
    ),
    "'true_alignmnent' must be a of class 'DNAbin'"
  )
})

test_that("mutation rate does not matter", {

  skip("Not now, Issue 294, Issue #294")
  # Long tips
  true_phylogeny  <- ape::read.tree(
    text = "(((((((((((A:90, B:90):1, C:91):1, D:92):1, E:93):1, F:94):1, G:95):1, H:96):1, I:97):1, J:98):1, K:99):1, L:100);"
  )
  twin_phylogeny  <- ape::read.tree(
    text = "(((((((((((A:1, B:1):1, C:2):1, D:3):1, E:4):1, F:5):1, G:6):1, H:7):1, I:8):1, J:9):1, K:10):90, L:100);"
  )
  ape::plot.phylo(true_phylogeny)
  ape::plot.phylo(twin_phylogeny)
  root_sequence <- create_blocked_dna(1000)
  alignment_params <- create_test_alignment_params(
    root_sequence = root_sequence,
    rng_seed = 314,
    mutation_rate = 10000.0
  )
  true_alignment <- sim_alignment(
    phylogeny = true_phylogeny,
    alignment_params = alignment_params
  )
  twinning_params <- create_twinning_params()
  twin_alignment <- create_twin_alignment(
    twin_phylogeny = twin_phylogeny,
    true_alignment = true_alignment,
    alignment_params = alignment_params,
    twinning_params = twinning_params
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
