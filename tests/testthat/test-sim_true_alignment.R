test_that("minimal use", {

  testthat::expect_silent(
    pirouette::sim_true_alignment(
      true_phylogeny = ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);"),
      alignment_params = pirouette::create_alignment_params()
    )
  )
})

test_that("inout is checked", {

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- pirouette::create_alignment_params()

  testthat::expect_error(
    pirouette::sim_true_alignment(
      true_phylogeny = "nonsense",
      alignment_params = alignment_params
    )
  )
  testthat::expect_error(
    pirouette::sim_true_alignment(
      true_phylogeny = phylogeny,
      alignment_params = "nonsense"
    )
  )
})

test_that("use linked_node_sub (lns)", {

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- pirouette::create_alignment_params(
    sim_tral_fun =
      pirouette::get_sim_tral_with_lns_nsm_fun(),
    root_sequence <- "aaccggtt"
  )
  alignment <- pirouette::sim_true_alignment(
    true_phylogeny = phylogeny,
    alignment_params = alignment_params,
  )
  testthat::expect_equal(nrow(alignment), ape::Ntip(phylogeny))
  testthat::expect_equal(ncol(alignment), nchar(root_sequence))
  # More detailed test are in
  # test-sim_tral_with_lns_nsm.R
})

test_that("use unlinked_node_sub (uns)", {

  root_sequence <- "aaaa"
  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- pirouette::create_alignment_params(
    sim_tral_fun =
      pirouette::get_sim_tral_with_uns_nsm_fun(),
    root_sequence = root_sequence
  )
  alignment <- pirouette::sim_true_alignment(
    true_phylogeny = phylogeny,
    alignment_params = alignment_params,
  )
  testthat::expect_equal(nrow(alignment), ape::Ntip(phylogeny))
  testthat::expect_equal(ncol(alignment), nchar(root_sequence))
  # More detailed test are in
  # test-sim_tral_with_uns_nsm.R
})
