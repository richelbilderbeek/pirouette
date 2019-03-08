#' @title Create the inference conditions
#' @description Create the parameters to determine how to choose
#' a model for the inference
#' @inheritParams default_params_doc
#' @return the inference conditions
#' @author Giovanni Laudanno
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
