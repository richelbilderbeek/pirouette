context("test-pir_run_tree")

test_that("use", {

  skip("#225")
  run_experiments <- pir_run_tree(
    phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
    alignment_params = create_test_alignment_params()
  )
  expect_silent(check_run_experiments(run_experiments))
})
