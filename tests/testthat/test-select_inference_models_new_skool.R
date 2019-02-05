context("test-select_inference_models_new_skool")

test_that("generative", {

  skip("Issue 69, #69")
  alignment_params <- create_alignment_params()
  model_select_params <- list(
      create_gen_model_select_param(
      alignment_params = alignment_params
    )
  )

  inference_models <- select_inference_models(
    alignment_params = alignment_params,
    model_select_params = as.list(seq(1, 314))
  )
  expect_equal(length(inference_models), 1)
  inference_model <- inference_models[[1]]
  expect_true("site_model" %in% names(inference_model))
  expect_true("clock_model" %in% names(inference_model))
  expect_true("tree_prior" %in% names(inference_model))
  expect_equal(inference_model$site_model, alignment_params$site_model)
  expect_equal(inference_model$clock_model, alignment_params$clock_model)
})

test_that("most_evidence", {

  skip("Issue 69, #69")

  alignment_params <- create_alignment_params()
  model_select_params <- list(create_best_model_select_param())

  # Fake a marg_liks
  marg_liks <- create_test_marg_liks()

  inference_models <- select_inference_models(
    alignment_params = alignment_params,
    model_select_params = as.list(seq(1, 314)),
    marg_liks = marg_liks
  )
  expect_equal(length(inference_models), 1)
  inference_model <- inference_models[[1]]

  most_evidence_row <- which(marg_liks$weight == max(marg_liks$weight))

  expect_equal(
    marg_liks$site_model_name[most_evidence_row],
    inference_model$site_model$name
  )
  expect_equal(
    marg_liks$clock_model_name[most_evidence_row],
    inference_model$clock_model$name
  )
  expect_equal(
    marg_liks$tree_prior_name[most_evidence_row],
    inference_model$tree_prior$name
  )
})

test_that("both: first generative, then most_evidence", {

  skip("Issue 69, #69")

  alignment_params <- create_alignment_params()
  model_select_params <- list(
    create_gen_model_select_param(
      alignment_params = alignment_params
    ),
    create_best_model_select_param()
  )
  marg_liks <- create_test_marg_liks()

  inference_models <- select_inference_models(
    alignment_params = alignment_params,
    model_select_params = as.list(seq(1, 314)),
    marg_liks = marg_liks
  )
  expect_equal(length(inference_models), length(model_select_params))
  inference_model <- inference_models[[1]]

  expect_equal(inference_model$site_model, alignment_params$site_model)
  expect_equal(inference_model$clock_model, alignment_params$clock_model)

  inference_model <- inference_models[[2]]

  most_evidence_row <- which(marg_liks$weight == max(marg_liks$weight))

  expect_equal(
    marg_liks$site_model_name[most_evidence_row],
    inference_model$site_model$name
  )
  expect_equal(
    marg_liks$clock_model_name[most_evidence_row],
    inference_model$clock_model$name
  )
  expect_equal(
    marg_liks$tree_prior_name[most_evidence_row],
    inference_model$tree_prior$name
  )
})
