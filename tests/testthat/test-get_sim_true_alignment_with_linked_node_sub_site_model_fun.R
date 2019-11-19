test_that("example", {
  f <- get_sim_true_alignment_with_linked_node_sub_site_model_fun()
  check_sim_true_alignment_fun(f)
  phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  true_alignment <- f(true_phylogeny = phylogeny, root_sequence = "acgt")
})

test_that("is valid function", {
  expect_silent(
    check_sim_true_alignment_fun(
      get_sim_true_alignment_with_linked_node_sub_site_model_fun()
    )
  )
})

test_that("usage", {
  alignment_params <- create_alignment_params(
    sim_true_alignment_fun =
      get_sim_true_alignment_with_linked_node_sub_site_model_fun(),
    root_sequence = "aaaaaaaa"
  )
  true_alignment <- pirouette::sim_true_alignment(
    true_phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
    alignment_params = alignment_params
  )
})
