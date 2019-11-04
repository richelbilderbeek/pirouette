test_that("minimal use", {

  # Beware: the 'n_mutations = 9' is due to the RNG always ending up
  # at 9 mutations.
  expect_silent(
    sim_alignment_with_n_mutations(
      phylogeny = ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);"),
      alignment_params = create_test_alignment_params(),
      n_mutations = 9
    )
  )
})

test_that("input is checked", {
  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- create_alignment_params()
  n_mutations <- 9

  expect_error(
    sim_alignment_with_n_mutations(
      phylogeny = "nonsense",
      alignment_params = alignment_params,
      n_mutations = n_mutations
    ),
    "phylogeny.*phylogeny"
  )
  expect_error(
    sim_alignment_with_n_mutations(
      phylogeny = phylogeny,
      alignment_params = "nonsense",
      n_mutations = n_mutations
    ),
    "alignment_params.*alignment_params"
  )
  expect_error(
    sim_alignment_with_n_mutations(
      phylogeny = phylogeny,
      alignment_params = alignment_params,
      n_mutations = "nonsense"
    ),
    "beautier::is_one_int.n_mutations. is not TRUE"
  )
  expect_error(
    sim_alignment_with_n_mutations(
      phylogeny = phylogeny,
      alignment_params = alignment_params,
      n_mutations = 1e123,
      verbose = TRUE
    ),
    "Cannot have more mutations than the total number of nucleotides"
  )
})

test_that("use linked_node_sub", {
  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- create_test_alignment_params(
    site_model = "linked_node_sub"
  )
  # Beware: the 'n_mutations = 2' is due to the RNG always ending up
  # at 2 mutations.
  alignment <- sim_alignment_with_n_mutations(
    phylogeny = phylogeny,
    alignment_params = alignment_params,
    n_mutations = 2,
    verbose  = TRUE
  )
  expect_equal(nrow(alignment), ape::Ntip(phylogeny))
  expect_equal(ncol(alignment), nchar(alignment_params$root_sequence))
})

test_that("use unlinked_node_sub", {
  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- create_test_alignment_params(
    site_model = "unlinked_node_sub"
  )
  # Beware: the 'n_mutations = 9' is due to the RNG always ending up
  # at 9 mutations.
  alignment <- sim_alignment_with_n_mutations(
    phylogeny = phylogeny,
    alignment_params = alignment_params,
    n_mutations = 9
  )
  expect_equal(nrow(alignment), ape::Ntip(phylogeny))
  expect_equal(ncol(alignment), nchar(alignment_params$root_sequence))
})

