context("test-twin_to_cbs_tree")

test_that("use", {

  expect_silent(
    twin_to_cbs_tree(
      phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
      twinning_params = create_twinning_params()
    )
  )
})
