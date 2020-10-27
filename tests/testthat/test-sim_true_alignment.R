test_that("minimal use", {

  expect_silent(
    pirouette::sim_true_alignment(
      true_phylogeny = ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);"),
      alignment_params = pirouette::create_alignment_params()
    )
  )
})

test_that("inout is checked", {

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- pirouette::create_alignment_params()

  expect_error(
    pirouette::sim_true_alignment(
      true_phylogeny = "nonsense",
      alignment_params = alignment_params
    )
  )
  expect_error(
    pirouette::sim_true_alignment(
      true_phylogeny = phylogeny,
      alignment_params = "nonsense"
    )
  )
})

# nodeSub is not on CRAN yet
#
# test_that("use linked_node_sub (lns)", {
#
#   phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);") # nolint
#   alignment_params <- pirouette::create_alignment_params( # nolint
#     sim_tral_fun = # nolint
#       pirouette::get_sim_tral_with_lns_nsm_fun(), # nolint
#     root_sequence <- "aaccggtt" # nolint
#   ) # nolint
#   alignment <- pirouette::sim_true_alignment( # nolint
#     true_phylogeny = phylogeny, # nolint
#     alignment_params = alignment_params, # nolint
#   ) # nolint
#   expect_equal(nrow(alignment), ape::Ntip(phylogeny)) # nolint
#   expect_equal(ncol(alignment), nchar(root_sequence)) # nolint
#   # More detailed test are in
#   # test-sim_tral_with_lns_nsm.R
# })

# nodeSub is not on CRAN yet
#
# test_that("use unlinked_node_sub (uns)", {
#
#   root_sequence <- "aaaa" # nolint
#   phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);") # nolint
#   alignment_params <- pirouette::create_alignment_params( # nolint
#     sim_tral_fun = # nolint
#       pirouette::get_sim_tral_with_uns_nsm_fun(), # nolint
#     root_sequence = root_sequence # nolint
#   ) # nolint
#   alignment <- pirouette::sim_true_alignment( # nolint
#     true_phylogeny = phylogeny, # nolint
#     alignment_params = alignment_params, # nolint
#   ) # nolint
#   expect_equal(nrow(alignment), ape::Ntip(phylogeny)) # nolint
#   expect_equal(ncol(alignment), nchar(root_sequence)) # nolint
#   # More detailed test are in
#   # test-sim_tral_with_uns_nsm.R
# })
