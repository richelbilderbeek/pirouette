context("test-create_ideal_tree")

test_that("use", {
  expect_true(
    beautier::is_phylo(
      create_ideal_tree(
        n_taxa = 5,
        crown_age = 10
      )
    )
  )
})
