context("test-check_pir_out")

test_that("simulated data", {
  if (!beastier::is_on_travis()) return()
  if (!beastier::is_beast2_installed()) return()

  pir_out <- pir_run(
    phylogeny =  phylogeny <- ape::read.tree(text = "((A:2, B:2):1, C:3);"),
    pir_params = create_test_pir_params(
      twinning_params = create_twinning_params(),
      experiments = list(
        create_test_gen_experiment(),
        create_test_cand_experiment()
      )
    )
  )

  expect_silent(check_pir_out(pir_out))
})


test_that("minimal use", {
  expect_silent(
    check_pir_out(
      pir_out = create_test_pir_run_output(
        add_best = TRUE,
        add_twin = TRUE
      )
    )
  )
})
