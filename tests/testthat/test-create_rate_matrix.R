context("test-create_rate_matrix")

test_that("abuse", {
  site_model <- create_site_model(
    name = "JC69",
    id = NA
  )
  site_model$name <- "nonsense"
  expect_error(
    create_rate_matrix(site_model = site_model),
    "'site_model' not implemented"
  )
})
