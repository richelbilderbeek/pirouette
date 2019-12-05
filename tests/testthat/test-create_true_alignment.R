test_that("basic", {
  n_taxa <- 3
  phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  testit::assert(length(phylogeny$tip.label) == n_taxa)
  sequence_length <- 10
  root_sequence <- create_mono_nuc_dna(length = sequence_length)
  alignment_params <- create_alignment_params(
    root_sequence = root_sequence
  )

  alignment <- create_true_alignment(
    true_phylogeny = phylogeny,
    alignment_params = alignment_params
  )
  expect_silent(check_alignment(alignment))
  expect_equal(
    get_alignment_n_taxa(alignment),
    n_taxa
  )
  expect_equal(
    get_alignment_sequence_length(alignment),
    sequence_length
  )
})

test_that("create_true_alignment: abuse", {

  phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  alignment_params <- create_alignment_params(
    root_sequence = create_mono_nuc_dna(length = 4)
  )

  expect_error(
    create_true_alignment(
      true_phylogeny = "not a phylogeny",
      alignment_params = alignment_params
    ),
    "'true_phylogeny' must be a valid phylogeny" #nolint
  )

  expect_error(
    create_true_alignment(
      true_phylogeny = phylogeny,
      alignment_params = "nonsense"
    ),
    "'alignment_params' must be a set of alignment parameters"
  )

  set.seed(42)
  p_with_extant <- ape::rtree(5)
  testit::assert(geiger::is.extinct(p_with_extant))

  expect_error(
    create_true_alignment(
      true_phylogeny = p_with_extant,
      alignment_params = alignment_params
    ),
    "A reconstructed phylogeny must not contain extinct species"
  )

})

test_that("low mutation rate must have less mutations", {
  phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  sequence_length <- 1000
  root_sequence <- create_mono_nuc_dna(length = sequence_length)
  alignment_params_low <- create_alignment_params(
    root_sequence = root_sequence,
    sim_true_alignment_fun =
      get_sim_true_alignment_with_std_nsm_fun(
        mutation_rate = 0.01
      )
  )
  alignment_params_high <- create_alignment_params(
    root_sequence = root_sequence,
    sim_true_alignment_fun =
      get_sim_true_alignment_with_std_nsm_fun(
        mutation_rate = 0.1
      )
  )
  alignment_low <- create_true_alignment(
    true_phylogeny = phylogeny,
    alignment_params = alignment_params_low
  )
  alignment_high <- create_true_alignment(
    true_phylogeny = phylogeny,
    alignment_params = alignment_params_high
  )
  n_mutations_low <- count_n_mutations(
    alignment = alignment_low,
    root_sequence = root_sequence
  )
  n_mutations_high <- count_n_mutations(
    alignment = alignment_high,
    root_sequence = root_sequence
  )
  expect_true(n_mutations_low * 5 < n_mutations_high)
})
