context("test-bd_phylo_2_l_table")

test_that("use", {
  phylogeny <- ape::read.tree(text = "((A:2, B:2):1, C:3);")

  l_table <- bd_phylo_2_l_table(phylogeny)

  expect_equal("matrix", class(l_table))
  expect_true("birth_time" %in% colnames(l_table))
  expect_true("parent" %in% colnames(l_table))
  expect_true("id" %in% colnames(l_table))
  expect_true("death_time" %in% colnames(l_table))
})

test_that("phylo -> L table -> phylogeny -> L table", {
  phylogeny <- ape::read.tree(text = "((A:2, B:2):1, C:3);")

  l_table <- bd_phylo_2_l_table(phylogeny)

  # Warning message:
  # In data.frame(..., check.names = FALSE) :
  #   row names were found from a short variable and have been discarded
  suppressWarnings(
    phylogeny_again <- DDD::L2phylo(l_table)
  )
  l_table_again <- bd_phylo_2_l_table(phylogeny_again)

  expect_equal(l_table, l_table_again)
})
