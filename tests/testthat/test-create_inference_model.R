context("test-create_old_skool_inference_model")

test_that("use", {
  expect_silent(
    check_old_skool_inference_model(
      create_old_skool_inference_model(
        site_model = beautier::create_jc69_site_model(),
        clock_model = beautier::create_strict_clock_model(),
        tree_prior = beautier::create_yule_tree_prior())
    )
  )
})
