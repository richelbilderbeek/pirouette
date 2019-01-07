context("test-create_tree_prior_from_name")

test_that("use", {
  names <- c(
    "yule",
    "birth_death",
    "coalescent_bayesian_skyline",
    "coalescent_constant_population",
    "coalescent_exp_population"
  )
  for (name in names) {
    expect_equal(name, create_tree_prior_from_name(name)$name)
  }
})
