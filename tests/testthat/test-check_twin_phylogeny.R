test_that("use", {
  expect_silent(
    check_twin_phylogeny(create_yule_tree())
  )
  expect_error(
    check_twin_phylogeny("nonsense"),
    "'twin_phylogeny' must be a valid phylogeny"
  )
  expect_error(check_twin_phylogeny(NA))
  expect_error(check_twin_phylogeny(NULL))
  expect_error(check_twin_phylogeny(c()))
  expect_error(check_twin_phylogeny(c(1, 2)))
})
