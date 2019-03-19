context("test-pir_table")

test_that("use", {
  expect_silent(
    pir_table(
      run_experiments = list(create_test_run_experiment())
    )
  )

  # Different number of errors in the results
  expect_silent(
    pir_table(
      run_experiments = list(
        create_test_run_experiment(
          true_result = create_test_result(errors = c(1, 2)),
          twin_result = create_test_result(errors = c(1, 2, 3))
        )
      )
    )
  )

  # One of the results' errors is NA
  expect_silent(
    pir_table(
      run_experiments = list(
        create_test_run_experiment(
          true_result = create_test_result(),
          twin_result = create_test_result(errors = NA)
        )
      )
    )
  )

  # One of the results' weights is NA
  expect_silent(
    pir_table(
      run_experiments = list(
        create_test_run_experiment(
          true_result = create_test_result(),
          twin_result = create_test_result(weight = NA)
        )
      )
    )
  )

  # One of the results' log_evidence is NA
  expect_silent(
    pir_table(
      run_experiments = list(
        create_test_run_experiment(
          true_result = create_test_result(),
          twin_result = create_test_result(log_evidence = NA)
        )
      )
    )
  )

  # True tree result is NA
  expect_silent(
    pir_table(
      run_experiments = list(
        create_test_run_experiment(
          true_result = NA,
          twin_result = create_test_result()
        )
      )
    )
  )

  # Twin tree result is NA
  expect_silent(
    pir_table(
      run_experiments = list(
        create_test_run_experiment(
          true_result = create_test_result(),
          twin_result = NA
        )
      )
    )
  )

  # Both tree results are NA
  expect_silent(
    pir_table(
      run_experiments = list(
        create_test_run_experiment(
          true_result = NA,
          twin_result = NA
        )
      )
    )
  )

  # Both of the results have only NAs
  expect_silent(
    pir_table(
      run_experiments = list(
        create_test_run_experiment(
          true_result = create_test_result(NA, NA, NA),
          twin_result = create_test_result(NA, NA, NA)
        )
      )
    )
  )
})
