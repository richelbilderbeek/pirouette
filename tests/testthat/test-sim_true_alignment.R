test_that("minimal use", {

  expect_silent(
    sim_true_alignment(
      true_phylogeny = ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);"),
      alignment_params = create_alignment_params()
    )
  )
})

test_that("inout is checked", {
  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- create_alignment_params()

  expect_error(
    sim_true_alignment(
      true_phylogeny = "nonsense",
      alignment_params = alignment_params
    )
  )
  expect_error(
    sim_true_alignment(
      true_phylogeny = phylogeny,
      alignment_params = "nonsense"
    )
  )
})

test_that("use linked_node_sub", {
  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- create_alignment_params(
    sim_true_alignment_function =
      get_sim_true_alignment_with_linked_node_sub_site_model_function(),
    root_sequence <- "aaccggtt"
  )
  alignment <- sim_true_alignment(
    true_phylogeny = phylogeny,
    alignment_params = alignment_params,
  )
  expect_equal(nrow(alignment), ape::Ntip(phylogeny))
  expect_equal(ncol(alignment), nchar(root_sequence))
  # More detailed test are in
  # test-sim_true_alignment_with_linked_node_sub_site_model.R
})

test_that("use unlinked_node_sub", {
  root_sequence <- "aaaa"
  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- create_alignment_params(
    sim_true_alignment_function = get_sim_true_alignment_with_unlinked_node_sub_site_model_function(
      root_sequence = root_sequence
    )
  )
  alignment <- sim_true_alignment(
    true_phylogeny = phylogeny,
    alignment_params = alignment_params,
  )
  expect_equal(nrow(alignment), ape::Ntip(phylogeny))
  expect_equal(ncol(alignment), nchar(root_sequence))
  # More detailed test are in
  # test-sim_true_alignment_with_unlinked_node_sub_site_model.R
})
