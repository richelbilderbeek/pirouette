#' @title Create the inference conditions
#' @description Create the parameters to determine how to choose
#' a model for the inference
#' @inheritParams default_params_doc
#' @return the inference conditions
#' @author Giovanni Laudanno
#' @examples
#'   library(testthat)
#'
#'   model_type <- "candidate"
#'   run_if <- "best_candidate"
#'   do_measure_evidence <- TRUE
#'
#'   inference_conditions <- create_inference_conditions(
#'     model_type = model_type,
#'     run_if = run_if,
#'     do_measure_evidence = do_measure_evidence
#'   )
#'
#'   expect_true("model_type" %in% names(inference_conditions))
#'   expect_true("run_if" %in% names(inference_conditions))
#'   expect_true("do_measure_evidence" %in% names(inference_conditions))
#' @export
create_inference_conditions <- function(
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
