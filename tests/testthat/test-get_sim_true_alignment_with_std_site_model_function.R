test_that("is valid function", {
  check_sim_true_alignment_function(
    get_sim_true_alignment_with_std_site_model_function(
      mutation_rate = 0.1
    )
  )
})

test_that("usage", {
  alignment_params <- create_alignment_params(
    sim_true_alignment_function =
      get_sim_true_alignment_with_std_site_model_function(
      mutation_rate = 0.5,
      site_model = beautier::create_hky_site_model()
    ),
    root_sequence = "aaaaaaaa"
  )
  true_alignment <- pirouette::sim_true_alignment(
    true_phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
    alignment_params = alignment_params
  )
})
