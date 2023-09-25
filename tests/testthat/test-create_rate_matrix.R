context("test-create_rate_matrix")

test_that("use, JC69", {
  created <- create_rate_matrix(site_model = beautier::create_jc69_site_model())
  expected <- NULL
  expect_equal(created, expected)
})

test_that("use, HKY", {
  created <- create_rate_matrix(site_model = beautier::create_hky_site_model())
  #       a     c     g     t
  # a -1.00  0.50  0.25  0.25
  # c  0.50 -1.00  0.25  0.25
  # g  0.25  0.25 -1.00  0.50
  # t  0.25  0.25  0.50 -1.00
  expected <- matrix(nrow = 4, ncol = 4)
  rownames(expected) <- c("a", "c", "g", "t")
  colnames(expected) <- c("a", "c", "g", "t")
  expected[1, ] <- c(-1.00, 0.50, 0.25, 0.25) # nolint align numbers nicely
  expected[2, ] <- c( 0.50,-1.00, 0.25, 0.25) # nolint align numbers nicely
  expected[3, ] <- c( 0.25, 0.25,-1.00, 0.50) # nolint align numbers nicely
  expected[4, ] <- c( 0.25, 0.25, 0.50,-1.00) # nolint align numbers nicely
  expect_equal(created, expected)
})

test_that("use, TN93", {
  created <- create_rate_matrix(site_model = beautier::create_tn93_site_model())
  #       a     c     g     t
  # a -1.00  0.50  0.25  0.25
  # c  0.50 -1.00  0.25  0.25
  # g  0.25  0.25 -1.00  0.50
  # t  0.25  0.25  0.50 -1.00
  expected <- matrix(nrow = 4, ncol = 4)
  rownames(expected) <- c("a", "c", "g", "t")
  colnames(expected) <- c("a", "c", "g", "t")
  expected[1, ] <- c(-1.00, 0.50, 0.25, 0.25) # nolint align numbers nicely
  expected[2, ] <- c( 0.50,-1.00, 0.25, 0.25) # nolint align numbers nicely
  expected[3, ] <- c( 0.25, 0.25,-1.00, 0.50) # nolint align numbers nicely
  expected[4, ] <- c( 0.25, 0.25, 0.50,-1.00) # nolint align numbers nicely
  expect_equal(created, expected)
})

test_that("use, GTR", {
  created <- create_rate_matrix(site_model = beautier::create_gtr_site_model())
  #       a     c     g     t
  # a -0.75  0.25  0.25  0.25
  # c  0.25 -0.75  0.25  0.25
  # g  0.25  0.25 -0.75  0.25
  # t  0.25  0.25  0.25 -0.75
  expected <- matrix(nrow = 4, ncol = 4)
  rownames(expected) <- c("a", "c", "g", "t")
  colnames(expected) <- c("a", "c", "g", "t")
  expected[1, ] <- c(-0.75, 0.25, 0.25, 0.25) # nolint align numbers nicely
  expected[2, ] <- c( 0.25,-0.75, 0.25, 0.25) # nolint align numbers nicely
  expected[3, ] <- c( 0.25, 0.25,-0.75, 0.25) # nolint align numbers nicely
  expected[4, ] <- c( 0.25, 0.25, 0.25,-0.75) # nolint align numbers nicely
  expect_equal(created, expected)
})

test_that("abuse", {
  site_model <- beautier::beautier::create_site_model(
    name = "JC69",
    id = NA
  )
  site_model$name <- "nonsense"
  expect_error(
    create_rate_matrix(site_model = site_model),
    "'site_model' not implemented"
  )
})
