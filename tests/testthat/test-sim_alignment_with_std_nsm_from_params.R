test_that("use", {
  phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  alignment_params <- create_alignment_params(
    root_sequence = "aaaa",
    sim_tral_fun = get_sim_tral_with_std_nsm_fun(
      mutation_rate = 0.1
    )
  )
  alignment <- sim_alignment_with_std_nsm_from_params(
    phylogeny = phylogeny,
    alignment_params = alignment_params
  )
  expect_silent(check_alignment(alignment))
})
