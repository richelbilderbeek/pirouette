test_that("minimal use", {

  expect_silent(
    sim_yule_twin_tree(ape::read.tree(text = "((A:2, B:2):1, C:3);"))
  )
  expect_silent(
    sim_yule_twin_tree(
      ape::read.tree(text = "((A:2, B:2):1, C:3);")
    )
  )
})

test_that("use", {
  phylogeny <- ape::read.tree(text = "((A:2, B:2):1, C:3);")

  bd_tree <- sim_yule_twin_tree(phylogeny)

  expect_equal(class(bd_tree), "phylo")

  # Branching times will differ, except the crown
  expect_false(
    all(
      ape::branching.times(phylogeny) ==
      ape::branching.times(bd_tree)
    )
  )

  # Crown age stays the same
  expect_equal(
    max(ape::branching.times(bd_tree)),
    max(ape::branching.times(phylogeny))
  )
})

test_that("abuse", {
  expect_error(
    sim_yule_twin_tree(
      true_phylogeny = ape::read.tree(text = "((A:2, B:2):1, C:3);"),
      method = "nonsense"
    ),
    "method"
  )
})
