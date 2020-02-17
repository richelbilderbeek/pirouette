test_that("use", {
  expect_silent(create_inference_conditions())

})

test_that("Cannot measure evidence on Windows", {

  expect_silent(
    create_inference_conditions(
      do_measure_evidence = FALSE,
      os = "win"
    )
  )

  expect_error(
    create_inference_conditions(
      do_measure_evidence = TRUE,
      os = "win"
    )
  )
})
