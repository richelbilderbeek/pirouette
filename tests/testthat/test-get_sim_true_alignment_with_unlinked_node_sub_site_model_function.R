test_that("example", {
  f <- get_sim_true_alignment_with_unlinked_node_sub_site_model_function(
    root_sequence = "acgt"
  )
  check_sim_true_alignment_function(f)
  phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  true_alignment <- f(true_phylogeny = phylogeny)
})

test_that("is valid function", {
  expect_silent(
    check_sim_true_alignment_function(
      get_sim_true_alignment_with_unlinked_node_sub_site_model_function(
        root_sequence = "acgt"
      )
    )
  )
})

test_that("usage", {
  root_sequence <- "aaaaaaaa"
  alignment_params <- create_alignment_params(
    sim_true_alignment_function =
      get_sim_true_alignment_with_unlinked_node_sub_site_model_function(
      root_sequence = "aaaaaaaa"
    ),
    root_sequence = root_sequence # To be obsoleted
  )
  true_alignment <- pirouette::sim_true_alignment(
    true_phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
    alignment_params = alignment_params
  )
})
