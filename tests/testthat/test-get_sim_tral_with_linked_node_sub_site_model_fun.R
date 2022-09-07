# nodeSub is not on CRAN yet
# test_that("example", {
#
#   f <- pirouette::get_sim_tral_with_lns_nsm_fun()
#   pirouette::check_sim_tral_fun(f)
#   phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
#   true_alignment <- f(true_phylogeny = phylogeny, root_sequence = "acgt")
#
# })
#
# test_that("is valid function", {
#
#
# })
#
# test_that("usage", {
#
#   alignment_params <- pirouette::create_alignment_params(
#     sim_tral_fun =
#       pirouette::get_sim_tral_with_lns_nsm_fun(),
#     root_sequence = "aaaaaaaa"
#   )
#   true_alignment <- pirouette::sim_true_alignment(
#     true_phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
#     alignment_params = alignment_params
#   )
#
# })
