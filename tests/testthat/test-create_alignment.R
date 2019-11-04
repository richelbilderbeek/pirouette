test_that("minimal use", {

  expect_silent(
    create_alignment(
      phylogeny = ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);"),
      alignment_params = create_alignment_params()
    )
  )
})

test_that("inout is checked", {
  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- create_alignment_params()

  expect_error(
    create_alignment(
      phylogeny = "nonsense",
      alignment_params = alignment_params
    )
  )
  expect_error(
    create_alignment(
      phylogeny = phylogeny,
      alignment_params = "nonsense"
    )
  )
})
