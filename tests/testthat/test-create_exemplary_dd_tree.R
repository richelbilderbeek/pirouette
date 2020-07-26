test_that("diversity_dependent, use", {
  expect_error(create_exemplary_dd_tree(n_taxa = -123456, crown_age = 10))
  expect_error(create_exemplary_dd_tree(n_taxa = 10, crown_age = -123456))
  expect_error(
    create_exemplary_dd_tree(
      n_taxa = 10, crown_age = 10, extinction_rate = -123.456
    )
  )
  expect_error(
    create_exemplary_dd_tree(
      n_taxa = 10, crown_age = 10, best_of_n_trees = -123.456
    )
  )

  expect_silent(create_exemplary_dd_tree(n_taxa = 2, crown_age = 1))
  expect_silent(
    create_exemplary_dd_tree(n_taxa = 10, crown_age = 1, extinction_rate = 0.0)
  )
  expect_silent(
    create_exemplary_dd_tree(n_taxa = 10, crown_age = 1, best_of_n_trees = 1)
  )
})

