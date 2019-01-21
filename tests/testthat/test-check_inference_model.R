context("test-check_inference_model")

test_that("use", {

  expect_silent(
    check_inference_model(
      create_inference_model(
        site_model = beautier::create_jc69_site_model(),
        clock_model = beautier::create_strict_clock_model(),
        tree_prior = beautier::create_yule_tree_prior()
      )
    )
  )
})

test_that("abuse", {
  inference_model <- create_inference_model(
    site_model = beautier::create_jc69_site_model(),
    clock_model = beautier::create_strict_clock_model(),
    tree_prior = beautier::create_yule_tree_prior()
  )
  inference_model2 <- list()
  inference_model2$site_model <- inference_model$site_model
  inference_model2$clock_model <- inference_model$clock_model
  expect_error(
    check_inference_model(
      inference_model2
    )
  )
})
