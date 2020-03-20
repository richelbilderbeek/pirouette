test_that("use", {

  phylogeny <- create_yule_tree(n_taxa = 3, crown_age = 15)
  expect_equal(
    create_standard_mutation_rate(phylogeny),
    1.0 / crown_age
  )
})

test_that("abuse", {

  expect_error(
    create_standard_mutation_rate(phylogeny = NA)
  )
  crown_age <- 10
  phylogeny <- create_yule_tree(n_taxa = 3, crown_age = 10)
  phylogeny$edge.length <- (phylogeny$edge.length * 10 ^ -305) * (1 / 10000) # nolint
  expect_error(
    create_standard_mutation_rate(phylogeny = phylogeny)
  )
})
