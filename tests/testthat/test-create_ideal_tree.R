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

test_that("harder", {

  if (!(beastier::is_on_ci())) {
    skip("This has to run on ci")
  }

  expect_true(
    beautier::is_phylo(
      create_ideal_tree(
        n_taxa = 100,
        crown_age = 30
      )
    )
  )
})
