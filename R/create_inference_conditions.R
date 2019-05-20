#' @title Create the inference conditions
#' @description Create the parameters to determine how to choose
#' a model for the inference
#' @inheritParams default_params_doc
#' @return the inference conditions
#' @author Giovanni Laudanno
#' @examples
#'  library(testthat)
#'
#'  # Create the inference conditions parameter set
#'  if (rappdirs::app_dir()$os != "win") {
#'    # it does not work on Windows
#'    # Model type can be 'generative' or 'candidate'
#'    model_type <- "candidate"
#'    # Run condition can be 'always' or 'best_candidate'
#'    run_if <- "best_candidate"
#'    # Evidence (aka marginal likelihood) can be measured yes or no
#'    do_measure_evidence <- TRUE
#'  } else {
#'    # Model type can be 'generative' or 'candidate'
#'    model_type <- "generative"
#'    # Run condition can be 'always' or 'best_candidate'
#'    run_if <- "always"
#'    # Evidence (aka marginal likelihood) can be measured yes or no
#'    do_measure_evidence <- FALSE
#'  }
#'
#'  inference_conditions <- create_inference_conditions(
#'    model_type = model_type,
#'    run_if = run_if,
#'    do_measure_evidence = do_measure_evidence
#'  )
#'  expect_true("model_type" %in% names(inference_conditions))
#'  expect_true("run_if" %in% names(inference_conditions))
#'  expect_true("do_measure_evidence" %in% names(inference_conditions))
#'
#'  # Using the inference conditions, create a testing candidate experiment
#'  experiment <- create_test_cand_experiment(
#'    inference_conditions = inference_conditions
#'  )
#'
#'  # Use the experiment to create the full pirouette parameter set
#'  pir_params <- create_pir_params(
#'    alignment_params = create_test_alignment_params(),
#'    experiments = list(experiment)
#'  )
#'
#'  # Run that experiment on a continuous integration service,
#'  # only when BEAST2 is unstalled
#'  if (is_on_ci() && is_beast2_installed() && is_beast2_pkg_installed("NS")) {
#'    pir_out <- pir_run(
#'      phylogeny = ape::read.tree(text = "((A:2, B:2):1, C:3);"),
#'      pir_params = pir_params
#'    )
#'    pir_plot(pir_out)
#'  }
#' @export
create_inference_conditions <- create_inf_conds <- function(
  model_type = "generative",
  run_if = "always",
  do_measure_evidence = FALSE
) {
  if (rappdirs::app_dir()$os == "win" && do_measure_evidence == TRUE) {
    stop("This configuration cannot run on windows")
  }
  inference_conditions <- list(
    model_type = model_type,
    run_if = run_if,
    do_measure_evidence = do_measure_evidence
  )
  check_inference_conditions(inference_conditions = inference_conditions) # nolint pirouette function
  inference_conditions
}
