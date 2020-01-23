test_that("use", {

  #
  #     +-------------- C                                                       # nolint this is no code
  # ----+                                                                       # nolint this is no code
  #     |    +--------- B                                                       # nolint this is no code
  #     +----+                                                                  # nolint this is no code
  #          +--------- A                                                       # nolint this is no code
  #
  expect_silent(
    check_reconstructed_phylogeny(
      ape::read.tree(text = "((A:2, B:2):1, C:3);")
    )
  )

  #
  #     +-------------- C                                                       # nolint this is no code
  # ----+                                                                       # nolint this is no code
  #     |    +--------- B                                                       # nolint this is no code
  #     +----+                                                                  # nolint this is no code
  #          +---- A                                                            # nolint this is no code
  #
  expect_error(
    check_reconstructed_phylogeny(
      ape::read.tree(text = "((A:1, B:2):1, C:3);")
    ),
    "A reconstructed phylogeny must not contain extinct species"
  )
})
