context("test-create_fig_bd")

test_that("use", {
  # Should return plot
  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  plot <- create_fig_bd(
    phylogeny = phylogeny,
    twinning_params = create_twinning_params(
      n_replicas = 10
    )
  )
  expect_true(
    all(sort(class(plot)) == sort(c("gg", "ggplot", "ggtree")))
  )
})

test_that("abuse", {
  expect_error(
    create_fig_bd("nonsense")
  )
})
