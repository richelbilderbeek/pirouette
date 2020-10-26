# nodeSub is not on CRAN yet

# test_that("example", {
#
#   f <- pirouette::get_sim_tral_with_uns_nsm_fun()
#   pirouette::check_sim_tral_fun(f)
#   phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
#   true_alignment <- f(true_phylogeny = phylogeny, root_sequence = "acgt")
#
# })
#
# test_that("is valid function", {
#
#   testthat::expect_silent(
#     pirouette::check_sim_tral_fun(
#       pirouette::get_sim_tral_with_uns_nsm_fun()
#     )
#   )
#
# })
#
# test_that("usage", {
#
#   alignment_params <- pirouette::create_alignment_params(
#     sim_tral_fun =
#       pirouette::get_sim_tral_with_uns_nsm_fun(),
#     root_sequence = "aaaaaaaa"
#   )
#   true_alignment <- pirouette::sim_true_alignment(
#     true_phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
#     alignment_params = alignment_params
#   )
#   testthat::expect_silent(pirouette::check_alignment_params(alignment_params))
#   testthat::expect_true(beastier::is_alignment(true_alignment))
#
# })
