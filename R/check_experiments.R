#' Checks if the argument is a list of one or more \link{pirouette} experiments.
#'
#' Will \link{stop} if not.
#' A valid \link{pirouette} experiment
#' can be created by \link{create_experiment}.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if not
#' @seealso Use \link{check_experiment} to check if an object
#'   is one valid experiment
#' @examples
#' if (beautier::is_on_ci()) {
#'
#'   check_experiments(list(create_test_experiment()))
#'
#'   if (rappdirs::app_dir()$os != "win") {
#'     experiments <- list(
#'       create_test_experiment(),
#'       create_test_cand_experiment()
#'     )
#'     # Experiments must have different inference models
#'     experiments[[1]]$inference_model$site_model <-
#'       beautier::create_gtr_site_model()
#'
#'     check_experiments(experiments)
#'   }
#' }
#' @author Richèl J.C. Bilderbeek
#' @export
check_experiments <- function(
  experiments
) {
  if (!is.list(experiments)) {
    stop(
      "'experiments' must be a list of one or more experiments.\n",
      "Actual value: ", experiments
    )
  }
  for (i in seq_along(experiments)) {
    tryCatch(
      pirouette::check_experiment(experiments[[i]]),
      error = function(e) {
        stop(
          "'experiments[[", i, "]] invalid.\n",
          "Tip: use 'create_experiment' for each list element.\n",
          "Error: ", e$message, "\n",
          "Value: ", experiments[[i]]
        )
      }
    )
  }
  if (length(experiments) == 1) return()

  testthat::expect_true(length(experiments) >= 2)
  pirouette::check_candidates_save_to_same_files(experiments)
  pirouette::check_experiments_candidates_have_same_mcmcs(experiments)

  model_types <- rep("", length(experiments))
  for (i in seq_along(experiments)) {
    model_types[i] <- experiments[[i]]$inference_conditions$model_type
  }
  if (sum(model_types == "generative") > 1) {
    stop("Specifying more than one 'generative' model experiment is redundant")
  }
  testthat::expect_true(length(experiments) >= 2)
  exp_types <- rep(NA, length(experiments))
  for (i in seq_along(experiments)) {
    exp_types[i] <- experiments[[i]]$inference_conditions$model_type
  }
  if (exp_types[1] != "generative" && ("generative" %in% exp_types)) {
    stop("If multiple experiments, generative is either first or absent")
  }
  pirouette::check_experiments_all_inference_models_are_unique(experiments)
  pirouette::check_gen_and_cand_exps_save_to_different_files(experiments)
  invisible(experiments)
}
