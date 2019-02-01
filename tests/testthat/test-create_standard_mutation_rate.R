context("test-create_standard_mutation_rate")

test_that("use", {
  crown_age <- 15
  phylogeny <- mbd::mbd_sim(
    pars = c(0.4, 0.1, 0, 0),
    n_0 = 2,
    age = crown_age,
    cond = 1,
    seed = 1
  )$reconstructed_tree
  expect_equal(
    create_standard_mutation_rate(phylogeny),
    1 / crown_age
  )
})

test_that("abuse", {
  expect_error(
    create_standard_mutation_rate(phylogeny = NA)
  )
  crown_age <- 10
  phylogeny <- mbd::mbd_sim(
    pars = c(0.4, 0.1, 0, 0),
    n_0 = 2,
    age = crown_age,
    cond = 1,
    seed = 1
  )$reconstructed_tree
  phylogeny$edge.length <- (phylogeny$edge.length * 10 ^ -305) * (1 / 10000) # nolint
  expect_error(
    create_standard_mutation_rate(phylogeny = phylogeny)
  )
})
