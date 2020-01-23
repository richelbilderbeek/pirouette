test_that("use", {

  #
  #     +-------------- C
  # ----+
  #     |    +--------- B
  #     +----+
  #          +--------- A
  #
  expect_silent(
    check_reconstructed_phylogeny(
      ape::read.tree(text = "((A:2, B:2):1, C:3);")
    )
  )

  #
  #     +-------------- C
  # ----+
  #     |    +--------- B
  #     +----+
  #          +---- A
  #
  expect_error(
    check_reconstructed_phylogeny(
      ape::read.tree(text = "((A:1, B:2):1, C:3);")
    ),
    "A reconstructed phylogeny must not contain extinct species"
  )
})
