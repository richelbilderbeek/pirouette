test_that("use on test output", {

  invisible(
    pir_plot(
      pir_out = create_test_pir_run_output(
        add_twin = FALSE,
        add_best = FALSE
      )
    )
  )
  expect_silent(
    pir_plot(
      pir_out = create_test_pir_run_output(
        add_twin = FALSE,
        add_best = TRUE
      )
    )
  )
  expect_silent(
    pir_plot(
      create_test_pir_run_output(
        add_twin = TRUE,
        add_best = FALSE
      )
    )
  )
  expect_silent(
    pir_plot(
      pir_out = create_test_pir_run_output(
        add_twin = TRUE,
        add_best = TRUE
      )
    )
  )

})

test_that("use on test runs", {

  if (!beautier::is_on_ci()) return()
  if (!beastier::is_beast2_installed()) return()

  pir_plot(
    pir_out = pir_run(
      phylogeny = create_test_phylogeny(),
      pir_params = create_test_pir_params_setup(
        has_twinning = FALSE,
        has_candidate = FALSE
      )
    )
  )
  pir_plot(
    pir_out = pir_run(
      phylogeny = create_test_phylogeny(),
      pir_params = create_test_pir_params_setup(
        has_twinning = TRUE,
        has_candidate = FALSE
      )
    )
  )

  # Runs with candidates
  if (rappdirs::app_dir()$os == "win") return()

  pir_plot(
    pir_out = pir_run(
      phylogeny = create_test_phylogeny(),
      pir_params = create_test_pir_params_setup(
        has_twinning = FALSE,
        has_candidate = TRUE
      )
    )
  )

  pir_plot(
    pir_out = pir_run(
      phylogeny = create_test_phylogeny(),
      pir_params = create_test_pir_params_setup(
        has_twinning = TRUE,
        has_candidate = TRUE
      )
    )
  )
})
